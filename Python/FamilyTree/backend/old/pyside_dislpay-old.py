import json
import sys
from pathlib import Path
from typing import Optional

from PySide6.QtCore import Qt, QRectF, QPointF, Signal, Slot
from PySide6.QtGui import QAction, QBrush, QColor, QFont, QPainter, QPainterPath, QPen, QPixmap
from PySide6.QtWidgets import (
    QApplication,
    QComboBox,
    QDialog,
    QFileDialog,
    QFormLayout,
    QFrame,
    QGraphicsItem,
    QGraphicsLineItem,
    QGraphicsObject,
    QGraphicsScene,
    QGraphicsView,
    QHBoxLayout,
    QInputDialog,
    QLabel,
    QLineEdit,
    QMainWindow,
    QMenu,
    QMessageBox,
    QPushButton,
    QSplitter,
    QStackedWidget,
    QToolBar,
    QVBoxLayout,
    QWidget,
)

from classes import Family, Person, family_to_json
from backend import (
    DATA_SOURCE,
    FAM_DIR,
    PPL_DIR,
    PPL_FILE,
    add_person,
    add_person_to_family,
    fill_new_dict,
    init_count_file,
    init_family,
    rmv_person,
    rmv_person_family,
    start,
    track_person_diseases,
    fam_blood_types,
    fam_diseases,
    upd_family_relations,
    upd_info_person,
)


# ---------------------------------------------------------------------
# GUI configuration
# ---------------------------------------------------------------------


class UIColors:
    BACKGROUND = "#000000"
    PANEL_BACKGROUND = "#050510"
    SELECTED_BACKGROUND = "#101040"
    HOVER_BACKGROUND = "#081830"
    HOVER_BORDER = "#ffff00"
    HOVER_GLOW = "#00ffff"
    PRIMARY = "#0044ff"
    PRIMARY_DARK = "#0033aa"
    SELECTED = "#00ffff"
    TEXT_SOFT = "#66aaff"
    PHOTO_LABEL_OVERLAY = (0, 0, 0, 170)


class PersonNodeStyle:
    WIDTH = 140
    HEIGHT = 90
    BORDER_WIDTH = 2
    FONT_NAME = "Arial"
    NAME_FONT_SIZE = 10
    ID_FONT_SIZE = 8
    PHOTO_INSET = 4
    PHOTO_LABEL_MARGIN = 10
    PHOTO_LABEL_HEIGHT = 18
    PHOTO_LABEL_BOTTOM_MARGIN = 28


class UnionNodeStyle:
    RADIUS = 5


class LayoutConfig:
    SCENE_X = -2500
    SCENE_Y = -2500
    SCENE_WIDTH = 5000
    SCENE_HEIGHT = 5000
    PERSON_X_SPACING = 190
    PERSON_Y_SPACING = 170
    PAN_STEP = 45
    ZOOM_FACTOR = 1.15
    DETAILS_MIN_WIDTH = 310
    DETAILS_MAX_WIDTH = 420
    WINDOW_WIDTH = 1150
    WINDOW_HEIGHT = 740


TOOLBAR_LAYOUT = [
    ["Initial screen"],
    ["Load family", "Add family"],
    ["Add person", "Load person to family"],
    ["Family blood types", "Family diseases"],
    ["Connect partners", "Connect father to child", "Connect mother to child"],
    ["Rmv partners", "Rmv father-child", "Rmv mother-child"],
]


# ---------------------------------------------------------------------
# Backend/file helpers for classes.py/backend.py
# ---------------------------------------------------------------------


def ensure_storage() -> None:
    start()
    init_count_file()


def people_file_path() -> Path:
    return Path(DATA_SOURCE) / PPL_DIR / PPL_FILE


def family_dir_path() -> Path:
    return Path(DATA_SOURCE) / FAM_DIR


def family_id_from_stem(stem: str) -> str:
    return stem.split("-", 1)[0]


def find_family_file(family_id: str) -> Optional[Path]:
    fam_dir = family_dir_path()
    if not fam_dir.exists():
        return None
    matches = sorted(fam_dir.glob(f"{family_id}-*.json"))
    return matches[0] if matches else None


def family_display_name_from_id(family_id: str) -> str:
    file_path = find_family_file(family_id)
    if file_path is None:
        return family_id
    try:
        with open(file_path, "r", encoding="utf-8") as f:
            data = json.load(f)
        return f"{data[family_id].get('name', file_path.stem)} ({family_id})"
    except Exception:
        return f"{file_path.stem} ({family_id})"


def available_family_names() -> list[str]:
    fam_dir = family_dir_path()
    if not fam_dir.exists():
        return []
    return sorted(path.stem for path in fam_dir.glob("*.json"))


def available_people_labels(people_by_id: dict[str, Person]) -> list[str]:
    return [f"{person.name} ({person.identifier})" for person in sorted(people_by_id.values(), key=lambda p: (p.name, p.identifier))]


def person_id_from_label(label: str) -> str:
    if "(" in label and label.endswith(")"):
        return label.rsplit("(", 1)[1][:-1]
    return label


def load_all_people() -> dict[str, Person]:
    path = people_file_path()
    if not path.exists():
        return {}

    with open(path, "r", encoding="utf-8") as f:
        data = json.load(f)

    people: dict[str, Person] = {}
    for identifier, p_data in data.items():
        people[identifier] = Person(
            name=p_data.get("name", ""),
            birth=p_data.get("birth", ""),
            death=p_data.get("death", ""),
            health_info=p_data.get("health_info", {}),
            photo=p_data.get("photo", ""),
            identifier=identifier,
            families=p_data.get("families", []),
        )
    return people


def load_family_by_id(family_id: str) -> Family:
    file_path = find_family_file(family_id)
    if file_path is None:
        raise FileNotFoundError(f"No family file found for {family_id}")
    return load_family_by_stem(file_path.stem)


def load_family_by_stem(stem: str) -> Family:
    family_id = family_id_from_stem(stem)
    file_path = find_family_file(family_id)
    if file_path is None:
        raise FileNotFoundError(f"No family file found for {family_id}")

    with open(file_path, "r", encoding="utf-8") as f:
        data = json.load(f)

    fam_data = data[family_id]
    return Family(
        name=fam_data.get("name", ""),
        identifier=family_id,
        members=fam_data.get("members", []),
        relations=fam_data.get("relations", {}),
    )


def reload_family(family: Family) -> Family:
    return load_family_by_id(family.identifier)


def save_family(family: Family) -> None:
    file_path = find_family_file(family.identifier)
    if file_path is None:
        safe_name = family.name.replace(" ", "_").lower()
        file_path = family_dir_path() / f"{family.identifier}-{safe_name}.json"

    with open(file_path, "w", encoding="utf-8") as f:
        json.dump(family_to_json(family), f, indent=4, ensure_ascii=False)


def normalize_and_save_family(family: Family) -> None:
    for person_id in list(family.members):
        family.relations.setdefault(person_id, fill_new_dict())
    family.relations = upd_family_relations(family.members, family.relations)
    save_family(family)


def person_label(people_by_id: dict[str, Person], person_id: Optional[str]) -> str:
    if not person_id:
        return "None"
    person = people_by_id.get(person_id)
    return f"{person.name} ({person.identifier})" if person else person_id


def get_relation(family: Family, person_id: str) -> dict:
    family.relations.setdefault(person_id, fill_new_dict())
    return family.relations[person_id]


def relation_list(family: Family, person_id: str, key: str) -> list[str]:
    rel = get_relation(family, person_id)
    value = rel.get(key)
    if value is None:
        rel[key] = []
        return rel[key]
    return value


def list_names(people_by_id: dict[str, Person], person_ids: list[str]) -> str:
    names = [people_by_id[pid].name if pid in people_by_id else pid for pid in person_ids]
    return ", ".join(names) if names else "None"


def couple_key(p1_id: str, p2_id: str) -> tuple[str, str]:
    return tuple(sorted([p1_id, p2_id]))


def add_partner_link(family: Family, p1_id: str, p2_id: str) -> bool:
    if p1_id == p2_id:
        return False
    changed = False
    p1_partners = relation_list(family, p1_id, "partners")
    p2_partners = relation_list(family, p2_id, "partners")
    if p2_id not in p1_partners:
        p1_partners.append(p2_id)
        changed = True
    if p1_id not in p2_partners:
        p2_partners.append(p1_id)
        changed = True
    return changed


def remove_partner_link(family: Family, p1_id: str, p2_id: str) -> bool:
    changed = False
    p1_partners = relation_list(family, p1_id, "partners")
    p2_partners = relation_list(family, p2_id, "partners")
    if p2_id in p1_partners:
        p1_partners.remove(p2_id)
        changed = True
    if p1_id in p2_partners:
        p2_partners.remove(p1_id)
        changed = True
    return changed


def remove_child_from_parent(family: Family, parent_id: Optional[str], child_id: str) -> None:
    if parent_id and parent_id in family.relations:
        kids = relation_list(family, parent_id, "kids")
        if child_id in kids:
            kids.remove(child_id)


def set_partner(family: Family, p1_id: str, p2_id: str) -> None:
    if add_partner_link(family, p1_id, p2_id):
        normalize_and_save_family(family)


def set_father(family: Family, child_id: str, father_id: str) -> None:
    if child_id == father_id:
        return
    child_rel = get_relation(family, child_id)
    remove_child_from_parent(family, child_rel.get("father"), child_id)
    child_rel["father"] = father_id
    father_kids = relation_list(family, father_id, "kids")
    if child_id not in father_kids:
        father_kids.append(child_id)
    mother_id = child_rel.get("mother")
    if mother_id and mother_id != father_id:
        add_partner_link(family, father_id, mother_id)
    normalize_and_save_family(family)


def set_mother(family: Family, child_id: str, mother_id: str) -> None:
    if child_id == mother_id:
        return
    child_rel = get_relation(family, child_id)
    remove_child_from_parent(family, child_rel.get("mother"), child_id)
    child_rel["mother"] = mother_id
    mother_kids = relation_list(family, mother_id, "kids")
    if child_id not in mother_kids:
        mother_kids.append(child_id)
    father_id = child_rel.get("father")
    if father_id and father_id != mother_id:
        add_partner_link(family, father_id, mother_id)
    normalize_and_save_family(family)


def remove_parent_child_connection(family: Family, p1_id: str, p2_id: str, parent_key: str) -> bool:
    changed = False
    p1_rel = get_relation(family, p1_id)
    p2_rel = get_relation(family, p2_id)
    if p2_rel.get(parent_key) == p1_id:
        p2_rel[parent_key] = None
        remove_child_from_parent(family, p1_id, p2_id)
        changed = True
    if p1_rel.get(parent_key) == p2_id:
        p1_rel[parent_key] = None
        remove_child_from_parent(family, p2_id, p1_id)
        changed = True
    if changed:
        normalize_and_save_family(family)
    return changed


def remove_partner_connection(family: Family, p1_id: str, p2_id: str) -> bool:
    changed = remove_partner_link(family, p1_id, p2_id)
    if changed:
        normalize_and_save_family(family)
    return changed


def remove_person_from_current_family(family: Family, person_id: str) -> None:
    rmv_person_family(family.identifier, person_id)
    ppl_path = people_file_path()
    if ppl_path.exists():
        with open(ppl_path, "r", encoding="utf-8") as f:
            data = json.load(f)
        if person_id in data and family.identifier in data[person_id].get("families", []):
            data[person_id]["families"].remove(family.identifier)
        with open(ppl_path, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=4, ensure_ascii=False)


def connection_mode_label(mode: str) -> str:
    return {
        "partner": "connect partners",
        "father": "connect father to child",
        "mother": "connect mother to child",
        "remove_partner": "remove partner connection",
        "remove_father": "remove father-child connection",
        "remove_mother": "remove mother-child connection",
    }.get(mode, mode)


# ---------------------------------------------------------------------
# Report helpers
# ---------------------------------------------------------------------


def format_disease_values(diseases) -> str:
    if not diseases:
        return "None"
    if isinstance(diseases, list):
        return ", ".join(str(disease) for disease in diseases) if diseases else "None"
    return str(diseases)


def format_people_entries(entries: list[tuple[str, str]]) -> str:
    return ", ".join(f"{name} ({person_id})" for name, person_id in entries) if entries else "None"


def format_person_disease_tracking(person_id: str, people_by_id: dict[str, Person]) -> str:
    tracking = track_person_diseases(person_id)
    if not tracking:
        return "No tracked relative diseases found."
    lines = []
    for relative_id, info in tracking.items():
        person = people_by_id.get(relative_id)
        relative_name = person.name if person is not None else relative_id
        lines.append(
            f"{relative_name} ({relative_id})\n"
            f"  Family: {info.get('fam', 'Unknown')}\n"
            f"  Relation: {info.get('relation', 'Unknown')}\n"
            f"  Degree: {info.get('degree', 'Unknown')}\n"
            f"  Diseases: {format_disease_values(info.get('disease', []))}"
        )
    return "\n\n".join(lines)


def format_family_blood_types_report(family_id: str) -> str:
    blood_types = fam_blood_types(family_id)
    if not blood_types:
        return "No blood type information found for this family."
    return "\n".join(f"{blood_type or 'Unknown'}: {format_people_entries(entries)}" for blood_type, entries in blood_types.items())


def format_family_diseases_report(family_id: str) -> str:
    diseases = fam_diseases(family_id)
    if not diseases:
        return "No diseases found for this family."
    return "\n".join(f"{disease or 'Unknown'}: {format_people_entries(entries)}" for disease, entries in diseases.items())


# ---------------------------------------------------------------------
# Dialogs
# ---------------------------------------------------------------------


class PersonDialog(QDialog):
    def __init__(self, parent=None, person: Optional[Person] = None):
        super().__init__(parent)
        self.setWindowTitle("Person information")
        health = person.health_info if person is not None and person.health_info else {}
        self.name_edit = QLineEdit(person.name if person else "")
        self.birth_edit = QLineEdit(person.birth if person else "")
        self.death_edit = QLineEdit(person.death if person else "")
        self.blood_type_edit = QLineEdit(health.get("blood_type", ""))
        self.diseases_edit = QLineEdit(", ".join(health.get("diseases", [])))
        self.photo_edit = QLineEdit(person.photo if person else "")
        self.photo_button = QPushButton("Browse")
        self.photo_button.clicked.connect(self.browse_photo)

        photo_row = QHBoxLayout()
        photo_row.addWidget(self.photo_edit)
        photo_row.addWidget(self.photo_button)

        form = QFormLayout()
        form.addRow("Name", self.name_edit)
        form.addRow("Birth", self.birth_edit)
        form.addRow("Death", self.death_edit)
        form.addRow("Blood type", self.blood_type_edit)
        form.addRow("Diseases comma-separated", self.diseases_edit)
        form.addRow("Photo", photo_row)

        ok = QPushButton("OK")
        cancel = QPushButton("Cancel")
        ok.clicked.connect(self.accept)
        cancel.clicked.connect(self.reject)
        buttons = QHBoxLayout()
        buttons.addStretch()
        buttons.addWidget(ok)
        buttons.addWidget(cancel)

        layout = QVBoxLayout(self)
        layout.addLayout(form)
        layout.addLayout(buttons)

    def browse_photo(self) -> None:
        selected, _ = QFileDialog.getOpenFileName(self, "Select photo", "", "Images (*.png *.jpg *.jpeg *.bmp *.gif)")
        if selected:
            self.photo_edit.setText(selected)

    def values(self) -> dict:
        diseases = [d.strip() for d in self.diseases_edit.text().split(",") if d.strip()]
        return {
            "name": self.name_edit.text().strip(),
            "birth": self.birth_edit.text().strip(),
            "death": self.death_edit.text().strip(),
            "health_info": {"blood_type": self.blood_type_edit.text().strip(), "diseases": diseases},
            "photo": self.photo_edit.text().strip(),
        }


class FamilySelectionDialog(QDialog):
    """Popup shown after selecting a person with one or more family memberships."""

    def __init__(self, parent, person: Person):
        super().__init__(parent)
        self.setWindowTitle(f"Families for {person.name}")
        self.person = person
        self.selected_family_id: Optional[str] = None

        label = QLabel(f"{person.name} belongs to these families. Choose one to load:")
        label.setWordWrap(True)
        self.family_combo = QComboBox()
        for family_id in person.families:
            self.family_combo.addItem(family_display_name_from_id(family_id), family_id)

        load_button = QPushButton("Load selected family")
        cancel_button = QPushButton("Cancel")
        load_button.clicked.connect(self.accept_selected)
        cancel_button.clicked.connect(self.reject)

        button_row = QHBoxLayout()
        button_row.addStretch()
        button_row.addWidget(load_button)
        button_row.addWidget(cancel_button)

        layout = QVBoxLayout(self)
        layout.addWidget(label)
        layout.addWidget(self.family_combo)
        layout.addLayout(button_row)

    def accept_selected(self) -> None:
        self.selected_family_id = self.family_combo.currentData()
        self.accept()


# ---------------------------------------------------------------------
# Graphics items
# ---------------------------------------------------------------------


class EdgeItem(QGraphicsLineItem):
    def __init__(self, source_item, target_item, pen: QPen):
        super().__init__()
        self.source_item = source_item
        self.target_item = target_item
        self.setPen(pen)
        self.setZValue(-10)
        self.source_item.add_edge(self)
        self.target_item.add_edge(self)
        self.update_position()

    def update_position(self) -> None:
        source_center = self.source_item.sceneBoundingRect().center()
        target_center = self.target_item.sceneBoundingRect().center()
        self.setLine(source_center.x(), source_center.y(), target_center.x(), target_center.y())


class UnionItem(QGraphicsObject):
    def __init__(self, p1_id: str, p2_id: str):
        super().__init__()
        self.p1_id = p1_id
        self.p2_id = p2_id
        self.connected_edges: list[EdgeItem] = []
        self.setFlags(QGraphicsItem.ItemIsMovable | QGraphicsItem.ItemIsSelectable | QGraphicsItem.ItemSendsGeometryChanges)

    def add_edge(self, edge: EdgeItem) -> None:
        self.connected_edges.append(edge)

    def itemChange(self, change, value):
        result = super().itemChange(change, value)
        if change == QGraphicsItem.ItemPositionHasChanged:
            for edge in list(self.connected_edges):
                edge.update_position()
        return result

    def boundingRect(self) -> QRectF:
        r = UnionNodeStyle.RADIUS
        return QRectF(-r, -r, 2 * r, 2 * r)

    def paint(self, painter: QPainter, option, widget=None) -> None:
        painter.setRenderHint(QPainter.Antialiasing)
        color = QColor(UIColors.SELECTED) if self.isSelected() else QColor(UIColors.PRIMARY)
        painter.setBrush(QBrush(color))
        painter.setPen(QPen(color, 1))
        painter.drawEllipse(self.boundingRect())


class PersonItem(QGraphicsObject):
    selected_person = Signal(str)
    hovered_person = Signal(str)
    right_clicked_person = Signal(str, object)

    def __init__(self, person: Person):
        super().__init__()
        self.person = person
        self.connected_edges: list[EdgeItem] = []
        self.photo_pixmap: Optional[QPixmap] = None
        self.is_hovered = False
        self.was_selected_before_hover = False
        self.normal_z_value = 0

        if self.person.photo:
            path = Path(self.person.photo)
            if path.exists():
                pixmap = QPixmap(str(path))
                if not pixmap.isNull():
                    self.photo_pixmap = pixmap

        self.setFlags(QGraphicsItem.ItemIsMovable | QGraphicsItem.ItemIsSelectable | QGraphicsItem.ItemSendsGeometryChanges)
        self.setAcceptHoverEvents(True)

    def add_edge(self, edge: EdgeItem) -> None:
        self.connected_edges.append(edge)

    def itemChange(self, change, value):
        result = super().itemChange(change, value)
        if change == QGraphicsItem.ItemPositionHasChanged:
            for edge in list(self.connected_edges):
                edge.update_position()
        return result

    def boundingRect(self) -> QRectF:
        return QRectF(0, 0, PersonNodeStyle.WIDTH, PersonNodeStyle.HEIGHT)

    def paint(self, painter: QPainter, option, widget=None) -> None:
        rect = self.boundingRect()
        if self.is_hovered:
            border = QColor(UIColors.HOVER_BORDER)
        elif self.isSelected():
            border = QColor(UIColors.SELECTED)
        else:
            border = QColor(UIColors.PRIMARY)
        fill = QColor(UIColors.SELECTED_BACKGROUND if self.isSelected() else UIColors.HOVER_BACKGROUND if self.is_hovered else UIColors.PANEL_BACKGROUND)

        painter.setRenderHint(QPainter.Antialiasing)
        painter.setBrush(QBrush(fill))
        painter.setPen(QPen(border, PersonNodeStyle.BORDER_WIDTH))
        painter.drawEllipse(rect.adjusted(1, 1, -1, -1))

        if self.photo_pixmap is not None:
            ellipse_path = QPainterPath()
            ellipse_path.addEllipse(rect.adjusted(PersonNodeStyle.PHOTO_INSET, PersonNodeStyle.PHOTO_INSET, -PersonNodeStyle.PHOTO_INSET, -PersonNodeStyle.PHOTO_INSET))
            painter.save()
            painter.setClipPath(ellipse_path)
            scaled = self.photo_pixmap.scaled(int(rect.width()), int(rect.height()), Qt.KeepAspectRatioByExpanding, Qt.SmoothTransformation)
            x = int(rect.x() + (rect.width() - scaled.width()) / 2)
            y = int(rect.y() + (rect.height() - scaled.height()) / 2)
            painter.drawPixmap(x, y, scaled)
            painter.restore()
            painter.setBrush(QBrush(QColor(*UIColors.PHOTO_LABEL_OVERLAY)))
            painter.setPen(Qt.NoPen)
            label_rect = QRectF(PersonNodeStyle.PHOTO_LABEL_MARGIN, PersonNodeStyle.HEIGHT - PersonNodeStyle.PHOTO_LABEL_BOTTOM_MARGIN, PersonNodeStyle.WIDTH - 2 * PersonNodeStyle.PHOTO_LABEL_MARGIN, PersonNodeStyle.PHOTO_LABEL_HEIGHT)
            painter.drawRoundedRect(label_rect, 8, 8)
            painter.setPen(QPen(QColor(UIColors.TEXT_SOFT)))
            painter.setFont(QFont(PersonNodeStyle.FONT_NAME, PersonNodeStyle.ID_FONT_SIZE, QFont.Bold))
            painter.drawText(label_rect, Qt.AlignCenter, self.person.name)
        else:
            painter.setPen(QPen(QColor(UIColors.TEXT_SOFT)))
            painter.setFont(QFont(PersonNodeStyle.FONT_NAME, PersonNodeStyle.NAME_FONT_SIZE, QFont.Bold))
            painter.drawText(rect.adjusted(12, 18, -12, -45), Qt.AlignCenter, self.person.name or "Unnamed")
            painter.setFont(QFont(PersonNodeStyle.FONT_NAME, PersonNodeStyle.ID_FONT_SIZE))
            painter.drawText(rect.adjusted(12, 45, -12, -18), Qt.AlignCenter, self.person.identifier)

        if self.is_hovered:
            painter.save()
            painter.setBrush(Qt.NoBrush)
            painter.setPen(QPen(QColor(UIColors.HOVER_GLOW), PersonNodeStyle.BORDER_WIDTH + 5))
            painter.drawEllipse(rect.adjusted(4, 4, -4, -4))
            painter.restore()

    def hoverEnterEvent(self, event) -> None:
        # Hover now only gives visual feedback. Person information is shown only
        # after an explicit left-click.
        self.is_hovered = True
        self.setZValue(20)
        self.update()
        super().hoverEnterEvent(event)

    def hoverLeaveEvent(self, event) -> None:
        self.is_hovered = False
        self.setZValue(self.normal_z_value)
        self.update()
        super().hoverLeaveEvent(event)

    def mousePressEvent(self, event) -> None:
        if event.button() == Qt.LeftButton and event.modifiers() & Qt.ControlModifier:
            self.right_clicked_person.emit(self.person.identifier, event.screenPos())
            event.accept()
            return
        if event.button() == Qt.LeftButton:
            self.selected_person.emit(self.person.identifier)
        super().mousePressEvent(event)

    def contextMenuEvent(self, event) -> None:
        self.right_clicked_person.emit(self.person.identifier, event.screenPos())


# ---------------------------------------------------------------------
# Scene and view
# ---------------------------------------------------------------------


class FamilyTreeScene(QGraphicsScene):
    person_selected = Signal(object)
    status_message = Signal(str)
    family_changed = Signal(object)
    person_disease_tracking_requested = Signal(str)
    family_navigation_requested = Signal(str, str)

    def __init__(self):
        super().__init__()
        self.setSceneRect(LayoutConfig.SCENE_X, LayoutConfig.SCENE_Y, LayoutConfig.SCENE_WIDTH, LayoutConfig.SCENE_HEIGHT)
        self.setBackgroundBrush(QBrush(QColor(UIColors.BACKGROUND)))
        self.family: Optional[Family] = None
        self.people_by_id: dict[str, Person] = {}
        self.person_items: dict[str, PersonItem] = {}
        self.union_items: dict[tuple[str, str], UnionItem] = {}
        self.edge_items: list[EdgeItem] = []
        self.mode: Optional[str] = None
        self.source_person_id: Optional[str] = None
        self.saved_person_positions: dict[str, QPointF] = {}
        self.saved_union_positions: dict[tuple[str, str], QPointF] = {}

    def remember_current_positions(self) -> None:
        for person_id, item in self.person_items.items():
            self.saved_person_positions[person_id] = item.pos()
        for key, item in self.union_items.items():
            self.saved_union_positions[key] = item.pos()

    def set_family(self, family: Family, people_by_id: dict[str, Person], preserve_positions: bool = True) -> None:
        if preserve_positions:
            self.remember_current_positions()
        self.family = family
        self.people_by_id = people_by_id
        self.rebuild()

    def clear_to_initial_screen(self) -> None:
        self.family = None
        self.people_by_id = {}
        self.clear()
        self.person_items.clear()
        self.union_items.clear()
        self.edge_items.clear()
        self.saved_person_positions.clear()
        self.saved_union_positions.clear()
        self.draw_initial_screen()

    def draw_initial_screen(self) -> None:
        self.clear()
        title = self.addText("FamilyTree", QFont(PersonNodeStyle.FONT_NAME, 24, QFont.Bold))
        title.setDefaultTextColor(QColor(UIColors.PRIMARY))
        title.setPos(-110, -130)
        hint = self.addText("Select an existing family or add a new one.", QFont(PersonNodeStyle.FONT_NAME, 11))
        hint.setDefaultTextColor(QColor(UIColors.PRIMARY))
        hint.setPos(-190, -80)

    def family_bounding_rect(self) -> QRectF:
        items = list(self.person_items.values()) + list(self.union_items.values()) + list(self.edge_items)
        if not items:
            return QRectF()
        rect = items[0].sceneBoundingRect()
        for item in items[1:]:
            rect = rect.united(item.sceneBoundingRect())
        return rect

    def rebuild(self) -> None:
        self.clear()
        self.person_items.clear()
        self.union_items.clear()
        self.edge_items.clear()
        if self.family is None:
            self.draw_initial_screen()
            return
        if not self.family.members:
            text = self.addText(f"Family: {self.family.name}\n\nNo members yet.\nUse 'Add person'.", QFont(PersonNodeStyle.FONT_NAME, 16))
            text.setDefaultTextColor(QColor(UIColors.PRIMARY))
            text.setPos(-160, -100)
            return
        self.create_person_items()
        self.draw_edges()

    def compute_tree_layout(self) -> dict[str, tuple[float, float]]:
        if self.family is None:
            return {}
        member_ids = [pid for pid in self.family.members if pid in self.people_by_id]
        if not member_ids:
            return {}
        levels: dict[str, int] = {}
        roots = [pid for pid in member_ids if not get_relation(self.family, pid).get("father") and not get_relation(self.family, pid).get("mother")]
        if not roots:
            roots = member_ids[:]
        for pid in roots:
            levels[pid] = 0
        for _ in range(max(4, len(member_ids) * 2)):
            changed = False
            for pid in member_ids:
                rel = get_relation(self.family, pid)
                parent_levels = [levels.get(parent, 0) for parent in (rel.get("father"), rel.get("mother")) if parent in member_ids]
                if parent_levels:
                    wanted = max(parent_levels) + 1
                    if levels.get(pid, 0) < wanted:
                        levels[pid] = wanted
                        changed = True
                else:
                    levels.setdefault(pid, 0)
            for pid in member_ids:
                for partner_id in relation_list(self.family, pid, "partners"):
                    if partner_id not in member_ids:
                        continue
                    wanted = max(levels.get(pid, 0), levels.get(partner_id, 0))
                    if levels.get(pid, 0) != wanted:
                        levels[pid] = wanted
                        changed = True
                    if levels.get(partner_id, 0) != wanted:
                        levels[partner_id] = wanted
                        changed = True
            if not changed:
                break
        people_by_level: dict[int, list[str]] = {}
        for pid in member_ids:
            people_by_level.setdefault(levels.get(pid, 0), []).append(pid)
        layout: dict[str, tuple[float, float]] = {}
        for level in sorted(people_by_level):
            row_ids = people_by_level[level]
            row_set = set(row_ids)
            placed: set[str] = set()
            ordered_groups: list[list[str]] = []
            for pid in row_ids:
                if pid in placed:
                    continue
                group = [pid]
                placed.add(pid)
                for partner_id in relation_list(self.family, pid, "partners"):
                    if partner_id in row_set and partner_id not in placed:
                        group.append(partner_id)
                        placed.add(partner_id)
                ordered_groups.append(group)
            total_slots = sum(len(group) for group in ordered_groups)
            start_x = -((total_slots - 1) * LayoutConfig.PERSON_X_SPACING) / 2
            slot = 0
            for group in ordered_groups:
                for pid in group:
                    layout[pid] = (start_x + slot * LayoutConfig.PERSON_X_SPACING, level * LayoutConfig.PERSON_Y_SPACING)
                    slot += 1
        return layout

    def create_person_items(self) -> None:
        if self.family is None:
            return
        layout = self.compute_tree_layout()
        for index, pid in enumerate(self.family.members):
            person = self.people_by_id.get(pid)
            if person is None:
                continue
            item = PersonItem(person)
            item.selected_person.connect(self.handle_person_clicked)
            # Hover is visual only; left-click controls the details panel.
            item.right_clicked_person.connect(self.open_person_menu)
            if pid in self.saved_person_positions:
                item.setPos(self.saved_person_positions[pid])
            else:
                item.setPos(*layout.get(pid, (index * LayoutConfig.PERSON_X_SPACING, 0)))
            self.addItem(item)
            self.person_items[pid] = item

    def add_edge_between(self, source_item, target_item, pen: QPen) -> None:
        edge = EdgeItem(source_item, target_item, pen)
        self.addItem(edge)
        self.edge_items.append(edge)

    def get_person_item(self, person_id: Optional[str]) -> Optional[PersonItem]:
        return self.person_items.get(person_id) if person_id else None

    def get_or_create_union_item(self, p1_id: str, p2_id: str) -> Optional[UnionItem]:
        p1_item = self.get_person_item(p1_id)
        p2_item = self.get_person_item(p2_id)
        if p1_item is None or p2_item is None:
            return None
        key = couple_key(p1_id, p2_id)
        if key in self.union_items:
            return self.union_items[key]
        union_item = UnionItem(p1_id, p2_id)
        if key in self.saved_union_positions:
            union_item.setPos(self.saved_union_positions[key])
        else:
            p1_center = p1_item.sceneBoundingRect().center()
            p2_center = p2_item.sceneBoundingRect().center()
            union_item.setPos((p1_center.x() + p2_center.x()) / 2, max(p1_center.y(), p2_center.y()) + 75)
        self.addItem(union_item)
        self.union_items[key] = union_item
        return union_item

    def draw_partner_union(self, p1_id: str, p2_id: str, partner_pen: QPen, drawn_partner_edges: set) -> Optional[UnionItem]:
        key = couple_key(p1_id, p2_id)
        union_item = self.get_or_create_union_item(p1_id, p2_id)
        p1_item = self.get_person_item(p1_id)
        p2_item = self.get_person_item(p2_id)
        if union_item is None or p1_item is None or p2_item is None:
            return None
        if key not in drawn_partner_edges:
            self.add_edge_between(p1_item, union_item, partner_pen)
            self.add_edge_between(p2_item, union_item, partner_pen)
            drawn_partner_edges.add(key)
        return union_item

    def draw_edges(self) -> None:
        if self.family is None:
            return
        partner_pen = QPen(QColor(UIColors.PRIMARY), 2)
        parent_pen = QPen(QColor(UIColors.PRIMARY_DARK), 1.5)
        drawn_partner_edges = set()
        drawn_children = set()
        member_set = set(self.family.members)
        for pid in self.family.members:
            for partner_id in relation_list(self.family, pid, "partners"):
                if partner_id in member_set:
                    self.draw_partner_union(pid, partner_id, partner_pen, drawn_partner_edges)
        for child_id in self.family.members:
            child_item = self.get_person_item(child_id)
            if child_item is None:
                continue
            rel = get_relation(self.family, child_id)
            father = rel.get("father") if rel.get("father") in member_set else None
            mother = rel.get("mother") if rel.get("mother") in member_set else None
            if father and mother:
                union_item = self.draw_partner_union(father, mother, partner_pen, drawn_partner_edges)
                child_key = (couple_key(father, mother), child_id)
                if union_item is not None and child_key not in drawn_children:
                    self.add_edge_between(union_item, child_item, parent_pen)
                    drawn_children.add(child_key)
            else:
                for parent in (father, mother):
                    parent_item = self.get_person_item(parent)
                    if parent_item is not None:
                        self.add_edge_between(parent_item, child_item, parent_pen)

    @Slot(str)
    def handle_person_hovered(self, person_id: str) -> None:
        # Kept for compatibility with older PersonItem signal wiring.
        # Hover no longer updates the details panel.
        return

    @Slot(str)
    def handle_person_clicked(self, person_id: str) -> None:
        if self.family is None:
            return
        person = self.people_by_id.get(person_id)
        if self.mode is not None:
            if self.source_person_id is None:
                self.source_person_id = person_id
                self.person_selected.emit(person)
                self.status_message.emit(f"{person.name if person else person_id} selected for {connection_mode_label(self.mode)}. Now click a different second person.")
                return
            if self.source_person_id == person_id:
                self.person_selected.emit(person)
                self.status_message.emit("The second node must be different from the first one. Press Esc to cancel.")
                return
            self.remember_current_positions()
            self.apply_connection(self.source_person_id, person_id)
            self.mode = None
            self.source_person_id = None
            self.family = reload_family(self.family)
            self.family_changed.emit(self.family)
            self.rebuild()
            self.person_selected.emit(self.people_by_id.get(person_id))
            return
        self.person_selected.emit(person)

    @Slot(str, object)
    def open_person_menu(self, person_id: str, screen_pos) -> None:
        person = self.people_by_id.get(person_id)
        if person is None:
            return

        self.person_selected.emit(person)
        menu = QMenu()

        family_actions = {}
        if person.families:
            families_menu = menu.addMenu("Load family")
            for family_id in person.families:
                action = families_menu.addAction(family_display_name_from_id(family_id))
                family_actions[action] = family_id
        else:
            no_families_action = menu.addAction("No families found")
            no_families_action.setEnabled(False)

        if menu.actions():
            menu.addSeparator()
        track_action = menu.addAction("Track diseases through relatives")

        chosen = menu.exec(screen_pos)
        if chosen is None:
            return
        if chosen == track_action:
            self.person_disease_tracking_requested.emit(person_id)
            return
        if chosen in family_actions:
            self.family_navigation_requested.emit(family_actions[chosen], person_id)

    def start_connection_mode(self, mode: str) -> None:
        self.mode = mode
        self.source_person_id = None
        self.status_message.emit(f"Click the first person to {connection_mode_label(mode)}.")

    def cancel_connection_mode(self) -> None:
        if self.mode is not None:
            self.mode = None
            self.source_person_id = None
            self.status_message.emit("Connection mode cancelled.")

    def apply_connection(self, source_id: str, target_id: str) -> None:
        if self.family is None:
            return
        source_name = self.people_by_id[source_id].name if source_id in self.people_by_id else source_id
        target_name = self.people_by_id[target_id].name if target_id in self.people_by_id else target_id
        if self.mode == "partner":
            set_partner(self.family, source_id, target_id)
            self.status_message.emit(f"{source_name} and {target_name} are now partners.")
        elif self.mode == "father":
            set_father(self.family, target_id, source_id)
            self.status_message.emit(f"{source_name} is now father of {target_name}.")
        elif self.mode == "mother":
            set_mother(self.family, target_id, source_id)
            self.status_message.emit(f"{source_name} is now mother of {target_name}.")
        elif self.mode == "remove_partner":
            msg = "Removed" if remove_partner_connection(self.family, source_id, target_id) else "No"
            self.status_message.emit(f"{msg} partner connection between {source_name} and {target_name}.")
        elif self.mode == "remove_father":
            msg = "Removed" if remove_parent_child_connection(self.family, source_id, target_id, "father") else "No"
            self.status_message.emit(f"{msg} father-child connection between {source_name} and {target_name}.")
        elif self.mode == "remove_mother":
            msg = "Removed" if remove_parent_child_connection(self.family, source_id, target_id, "mother") else "No"
            self.status_message.emit(f"{msg} mother-child connection between {source_name} and {target_name}.")


class FamilyTreeView(QGraphicsView):
    shortcuts_requested = Signal()

    def __init__(self, scene: FamilyTreeScene):
        super().__init__(scene)
        self.setRenderHints(QPainter.Antialiasing | QPainter.TextAntialiasing)
        self.setDragMode(QGraphicsView.RubberBandDrag)
        self.setViewportUpdateMode(QGraphicsView.BoundingRectViewportUpdate)
        self.setTransformationAnchor(QGraphicsView.AnchorUnderMouse)
        self.setFocusPolicy(Qt.StrongFocus)

    def zoom(self, factor: float) -> None:
        self.scale(factor, factor)

    def center_family_graph(self) -> None:
        scene = self.scene()
        if isinstance(scene, FamilyTreeScene):
            rect = scene.family_bounding_rect()
            if not rect.isNull():
                self.fitInView(rect.adjusted(-80, -80, 80, 80), Qt.KeepAspectRatio)

    def pan(self, dx: int, dy: int) -> None:
        self.horizontalScrollBar().setValue(self.horizontalScrollBar().value() + dx)
        self.verticalScrollBar().setValue(self.verticalScrollBar().value() + dy)

    def wheelEvent(self, event) -> None:
        self.zoom(LayoutConfig.ZOOM_FACTOR if event.angleDelta().y() > 0 else 1 / LayoutConfig.ZOOM_FACTOR)

    def keyPressEvent(self, event) -> None:
        key = event.key()
        if key == Qt.Key_H:
            self.shortcuts_requested.emit()
            event.accept()
            return
        if key == Qt.Key_C:
            self.center_family_graph()
            event.accept()
            return
        if key in (Qt.Key_Plus, Qt.Key_Equal):
            self.zoom(LayoutConfig.ZOOM_FACTOR)
            event.accept()
            return
        if key == Qt.Key_Minus:
            self.zoom(1 / LayoutConfig.ZOOM_FACTOR)
            event.accept()
            return
        if key == Qt.Key_Escape and isinstance(self.scene(), FamilyTreeScene):
            self.scene().cancel_connection_mode()
            event.accept()
            return
        if key in (Qt.Key_Left, Qt.Key_Right, Qt.Key_Up, Qt.Key_Down):
            dx = LayoutConfig.PAN_STEP if key == Qt.Key_Right else -LayoutConfig.PAN_STEP if key == Qt.Key_Left else 0
            dy = LayoutConfig.PAN_STEP if key == Qt.Key_Down else -LayoutConfig.PAN_STEP if key == Qt.Key_Up else 0
            self.pan(dx, dy)
            event.accept()
            return
        super().keyPressEvent(event)


# ---------------------------------------------------------------------
# Panels/pages
# ---------------------------------------------------------------------


class InitialPage(QWidget):
    add_family_requested = Signal()
    family_selected = Signal(str)

    def __init__(self):
        super().__init__()
        self.title = QLabel("FamilyTree")
        self.title.setAlignment(Qt.AlignCenter)
        self.title.setStyleSheet(f"font-size: 28px; font-weight: bold; color: {UIColors.PRIMARY};")
        self.subtitle = QLabel("Select an existing family or add a new one.")
        self.subtitle.setAlignment(Qt.AlignCenter)
        self.family_combo = QComboBox()
        self.family_combo.setMinimumWidth(300)
        self.load_button = QPushButton("Show selected family")
        self.add_button = QPushButton("Add family")
        self.refresh_button = QPushButton("Refresh list")
        self.load_button.clicked.connect(self.emit_selected_family)
        self.add_button.clicked.connect(self.add_family_requested.emit)
        self.refresh_button.clicked.connect(self.refresh_families)
        button_row = QHBoxLayout()
        button_row.addWidget(self.load_button)
        button_row.addWidget(self.add_button)
        button_row.addWidget(self.refresh_button)
        box = QVBoxLayout()
        box.addWidget(self.title)
        box.addWidget(self.subtitle)
        box.addSpacing(20)
        #box.addWidget(QLabel("Select family"))
        box.addWidget(self.family_combo)
        box.addLayout(button_row)
        outer = QVBoxLayout(self)
        outer.addStretch()
        outer.addLayout(box)
        outer.addStretch()
        self.refresh_families()

    def refresh_families(self) -> None:
        self.family_combo.clear()
        self.family_combo.addItem("Select family", "")
        for name in available_family_names():
            self.family_combo.addItem(name, name)

    def emit_selected_family(self) -> None:
        selected = self.family_combo.currentData()
        if selected:
            self.family_selected.emit(selected)


class DetailsPanel(QFrame):
    add_person_requested = Signal()
    edit_person_requested = Signal(object)
    delete_person_requested = Signal(object)
    remove_person_requested = Signal(object)
    connection_requested = Signal(str)

    def __init__(self):
        super().__init__()
        self.family: Optional[Family] = None
        self.people_by_id: dict[str, Person] = {}
        self.current_person: Optional[Person] = None
        self.setMinimumWidth(LayoutConfig.DETAILS_MIN_WIDTH)
        self.setMaximumWidth(LayoutConfig.DETAILS_MAX_WIDTH)
        self.title = QLabel("No person selected")
        self.title.setObjectName("detailsTitle")
        self.info = QLabel("Select a person node.")
        self.info.setObjectName("detailsInfo")
        self.info.setWordWrap(True)
        self.edit_button = QPushButton("Edit person")
        self.remove_from_family_button = QPushButton("Remove from the current family")
        self.remove_person_button = QPushButton("Remove person")
        self.edit_button.clicked.connect(self.emit_edit)
        self.remove_from_family_button.clicked.connect(self.emit_delete)
        self.remove_person_button.clicked.connect(self.emit_remove_person)
        self.person_action_buttons = [
            self.edit_button,
            self.remove_from_family_button,
            self.remove_person_button,
        ]
        for button in self.person_action_buttons:
            button.hide()
        layout = QVBoxLayout(self)
        layout.addWidget(self.title)
        layout.addWidget(self.info)
        layout.addSpacing(8)
        layout.addWidget(self.edit_button)
        layout.addWidget(self.remove_from_family_button)
        layout.addWidget(self.remove_person_button)
        layout.addStretch()

    def set_context(self, family: Optional[Family], people_by_id: dict[str, Person]) -> None:
        self.family = family
        self.people_by_id = people_by_id
        self.set_person(self.current_person if self.current_person else None)

    def set_person_action_buttons_visible(self, visible: bool) -> None:
        for button in self.person_action_buttons:
            button.setVisible(visible)

    def set_person(self, person: Optional[Person]) -> None:
        self.current_person = person
        if person is None:
            self.title.setText("No person selected")
            self.info.setText("Select a person node.")
            self.set_person_action_buttons_visible(False)
            return
        self.set_person_action_buttons_visible(True)
        rel = get_relation(self.family, person.identifier) if self.family is not None and person.identifier in self.family.members else fill_new_dict()
        health = person.health_info or {}
        diseases = health.get("diseases", [])
        families = person.families if person.families else []
        self.title.setText(person.name)
        self.info.setText(
            f"ID: {person.identifier}\n"
            f"Families: {', '.join(families) if families else 'None'}\n\n"
            f"Personal information\n"
            f"Birth: {person.birth or 'Unknown'}\n"
            f"Death: {person.death or 'Not yet'}\n"
            f"Blood type: {health.get('blood_type', '') or 'Unknown'}\n"
            f"Diseases: {', '.join(diseases) if diseases else 'None'}\n\n"
            f"Family relations\n"
            f"Father: {person_label(self.people_by_id, rel.get('father'))}\n"
            f"Mother: {person_label(self.people_by_id, rel.get('mother'))}\n"
            f"Partners: {list_names(self.people_by_id, rel.get('partners', []))}\n"
            f"Kids: {list_names(self.people_by_id, rel.get('kids', []))}\n"
            f"Siblings: {list_names(self.people_by_id, rel.get('siblings', []))}"
        )

    def display_report(self, title: str, text: str, current_person: Optional[Person] = None) -> None:
        self.current_person = current_person
        self.title.setText(title)
        self.info.setText(text)
        self.set_person_action_buttons_visible(False)

    def emit_edit(self) -> None:
        if self.current_person is not None:
            self.edit_person_requested.emit(self.current_person)

    def emit_delete(self) -> None:
        if self.current_person is not None:
            self.delete_person_requested.emit(self.current_person)

    def emit_remove_person(self) -> None:
        if self.current_person is not None:
            self.remove_person_requested.emit(self.current_person)


# ---------------------------------------------------------------------
# Main window
# ---------------------------------------------------------------------


class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        ensure_storage()
        self.setWindowTitle("FamilyTree")
        self.resize(LayoutConfig.WINDOW_WIDTH, LayoutConfig.WINDOW_HEIGHT)
        self.family: Optional[Family] = None
        self.people_by_id: dict[str, Person] = load_all_people()

        self.scene = FamilyTreeScene()
        self.view = FamilyTreeView(self.scene)
        self.details = DetailsPanel()
        self.scene.person_selected.connect(self.details.set_person)
        self.scene.status_message.connect(self.statusBar().showMessage)
        self.scene.family_changed.connect(self.set_family_from_scene)
        self.scene.person_disease_tracking_requested.connect(self.show_person_disease_tracking)
        self.scene.family_navigation_requested.connect(self.load_family_by_id_from_gui)
        self.view.shortcuts_requested.connect(self.show_shortcuts)
        self.details.edit_person_requested.connect(self.edit_person)
        self.details.delete_person_requested.connect(self.delete_person)
        self.details.remove_person_requested.connect(self.remove_person)

        self.initial_page = InitialPage()
        self.initial_page.add_family_requested.connect(self.create_family)
        self.initial_page.family_selected.connect(self.load_family_by_name)

        self.graph_page = QSplitter()
        self.graph_page.addWidget(self.view)
        self.graph_page.addWidget(self.details)
        self.graph_page.setStretchFactor(0, 1)
        self.graph_page.setStretchFactor(1, 0)

        self.stack = QStackedWidget()
        self.stack.addWidget(self.initial_page)
        self.stack.addWidget(self.graph_page)
        self.setCentralWidget(self.stack)

        self.create_toolbar()
        self.scene.draw_initial_screen()
        self.show_initial_page()
        self.apply_style()

    def apply_style(self) -> None:
        self.setStyleSheet(f"""
            QMainWindow {{ background-color: {UIColors.BACKGROUND}; }}
            QStatusBar {{ color: {UIColors.PRIMARY}; background-color: {UIColors.PANEL_BACKGROUND}; border-top: 1px solid {UIColors.PRIMARY}; }}
            QToolBar {{ background-color: {UIColors.PANEL_BACKGROUND}; border-bottom: 1px solid {UIColors.PRIMARY}; spacing: 8px; }}
            QToolButton {{ color: {UIColors.PRIMARY}; background-color: {UIColors.BACKGROUND}; border: 1px solid {UIColors.PRIMARY}; padding: 6px; }}
            QToolButton:hover {{ background-color: {UIColors.SELECTED_BACKGROUND}; }}
            QMenu {{ background-color: {UIColors.PANEL_BACKGROUND}; color: {UIColors.PRIMARY}; border: 1px solid {UIColors.PRIMARY}; }}
            QDialog {{ background-color: {UIColors.PANEL_BACKGROUND}; color: {UIColors.PRIMARY}; }}
            QLabel {{ color: {UIColors.PRIMARY}; }}
            QLabel#detailsTitle {{ font-size: 18px; font-weight: bold; color: {UIColors.PRIMARY}; }}
            QLabel#detailsInfo {{ color: {UIColors.PRIMARY}; }}
            QLineEdit, QComboBox {{ color: {UIColors.PRIMARY}; background-color: {UIColors.BACKGROUND}; border: 1px solid {UIColors.PRIMARY}; padding: 4px; }}
            QPushButton {{ color: {UIColors.PRIMARY}; background-color: {UIColors.BACKGROUND}; border: 1px solid {UIColors.PRIMARY}; padding: 6px; }}
            QPushButton:hover {{ background-color: {UIColors.SELECTED_BACKGROUND}; }}
        """)

    def toolbar_action_map(self) -> dict[str, QAction]:
        actions = {
            "Initial screen": QAction("Initial screen", self),
            "Load family": QAction("Load family", self),
            "Add family": QAction("Add family", self),
            "Add person": QAction("Add person", self),
            "Load person to family": QAction("Load person to family", self),
            "Rmv person": QAction("Rmv person", self),
            "Rmv person from family": QAction("Rmv person from family", self),
            "Family blood types": QAction("Family blood types", self),
            "Family diseases": QAction("Family diseases", self),
            "Connect partners": QAction("Connect partners", self),
            "Connect father to child": QAction("Connect father to child", self),
            "Connect mother to child": QAction("Connect mother to child", self),
            "Rmv partners": QAction("Rmv partners", self),
            "Rmv father-child": QAction("Rmv father-child", self),
            "Rmv mother-child": QAction("Rmv mother-child", self),
        }
        actions["Initial screen"].triggered.connect(self.initial_screen)
        actions["Load family"].triggered.connect(self.load_existing_family)
        actions["Add family"].triggered.connect(self.create_family)
        actions["Add person"].triggered.connect(self.add_person)
        actions["Load person to family"].triggered.connect(self.load_person_to_family)
        actions["Rmv person"].triggered.connect(self.remove_selected_person)
        actions["Rmv person from family"].triggered.connect(self.remove_selected_person_from_family)
        actions["Family blood types"].triggered.connect(self.show_family_blood_types)
        actions["Family diseases"].triggered.connect(self.show_family_diseases)
        actions["Connect partners"].triggered.connect(lambda: self.start_two_click_connection("partner"))
        actions["Connect father to child"].triggered.connect(lambda: self.start_two_click_connection("father"))
        actions["Connect mother to child"].triggered.connect(lambda: self.start_two_click_connection("mother"))
        actions["Rmv partners"].triggered.connect(lambda: self.start_two_click_connection("remove_partner"))
        actions["Rmv father-child"].triggered.connect(lambda: self.start_two_click_connection("remove_father"))
        actions["Rmv mother-child"].triggered.connect(lambda: self.start_two_click_connection("remove_mother"))
        return actions

    def create_toolbar(self) -> None:
        toolbar = QToolBar("Main toolbar")
        self.addToolBar(toolbar)

        shortcuts_action = QAction("Show shortcuts", self)
        shortcuts_action.setShortcut("H")
        shortcuts_action.setShortcutContext(Qt.ApplicationShortcut)
        shortcuts_action.triggered.connect(self.show_shortcuts)
        self.addAction(shortcuts_action)

        actions = self.toolbar_action_map()
        for group in TOOLBAR_LAYOUT:
            for name in group:
                toolbar.addAction(actions[name])
            toolbar.addSeparator()

    def refresh_people(self) -> None:
        self.people_by_id = load_all_people()

    def show_initial_page(self) -> None:
        self.initial_page.refresh_families()
        self.stack.setCurrentWidget(self.initial_page)

    def show_graph_page(self) -> None:
        self.stack.setCurrentWidget(self.graph_page)
        self.view.setFocus()

    def set_active_family(self, family: Family, preserve_positions: bool = False) -> None:
        self.family = family
        self.refresh_people()
        if not preserve_positions:
            self.scene.saved_person_positions.clear()
            self.scene.saved_union_positions.clear()
        self.scene.set_family(self.family, self.people_by_id, preserve_positions=preserve_positions)
        self.details.set_context(self.family, self.people_by_id)
        self.details.set_person(None)
        self.show_graph_page()
        self.view.center_family_graph()

    @Slot(object)
    def set_family_from_scene(self, family: Family) -> None:
        self.set_active_family(family, preserve_positions=True)

    @Slot()
    def create_family(self) -> None:
        name, ok = QInputDialog.getText(self, "Create family", "Family name:")
        if not ok or not name.strip():
            return
        family = init_family(name.strip())
        save_family(family)
        self.set_active_family(family)
        self.initial_page.refresh_families()
        self.statusBar().showMessage(f"Created family: {self.family.name}")

    @Slot(str)
    def load_family_by_name(self, selected: str) -> None:
        if not selected:
            return
        family = load_family_by_stem(selected)
        self.set_active_family(family)
        self.statusBar().showMessage(f"Loaded family: {self.family.name}")

    def load_family_by_id_from_gui(self, family_id: str, selected_person_id: Optional[str] = None) -> None:
        family = load_family_by_id(family_id)
        self.set_active_family(family)
        if selected_person_id and selected_person_id in self.people_by_id:
            self.details.set_person(self.people_by_id[selected_person_id])
        self.statusBar().showMessage(f"Loaded family: {self.family.name}")

    @Slot()
    def load_existing_family(self) -> None:
        choices = available_family_names()
        if not choices:
            QMessageBox.information(self, "No families found", f"No family files found in:\n{family_dir_path()}")
            return
        selected, ok = QInputDialog.getItem(self, "Load family", "Choose a family:", choices, 0, False)
        if ok and selected:
            self.load_family_by_name(selected)

    @Slot()
    def initial_screen(self) -> None:
        self.family = None
        self.scene.clear_to_initial_screen()
        self.details.set_context(None, self.people_by_id)
        self.details.set_person(None)
        self.show_initial_page()

    @Slot(object)
    def handle_person_selected_for_family_navigation(self, person: Optional[Person]) -> None:
        # Kept for compatibility with older signal wiring. Family navigation now
        # lives in the Ctrl+left-click context menu.
        if person is None or not person.families:
            return
        dialog = FamilySelectionDialog(self, person)
        if dialog.exec() == QDialog.Accepted and dialog.selected_family_id:
            self.load_family_by_id_from_gui(dialog.selected_family_id, selected_person_id=person.identifier)

    @Slot()
    def show_shortcuts(self) -> None:
        QMessageBox.information(
            self,
            "Shortcuts",
            "Keyboard shortcuts:\n\n"
            "h: show this shortcuts window\n"
            "c: center graph\n"
            "+: zoom in\n"
            "-: zoom out\n"
            "Arrow keys: pan graph\n"
            "Esc: cancel connection mode\n\n"
            "Mouse shortcuts:\n\n"
            "Hover person: highlight person only\n"
            "Left-click person: show information / use in connection mode\n"
            "Ctrl + left-click person: open person context menu\n"
            "Right-click person: open person context menu\n"
            "Mouse wheel: zoom graph\n\n"
            "Person context menu:\n\n"
            "Load one of the person's families, or track diseases through relatives."
        )

    @Slot()
    def add_person(self) -> None:
        if self.family is None:
            QMessageBox.information(self, "No family", "Create or load a family first.")
            return
        dialog = PersonDialog(self)
        if dialog.exec() != QDialog.Accepted:
            return
        values = dialog.values()
        if not values["name"]:
            QMessageBox.warning(self, "Invalid person", "Name cannot be empty.")
            return
        person = add_person(
            values["name"],
            birth=values["birth"],
            death=values["death"],
            blood_type=values["health_info"].get("blood_type"),
            diseases=values["health_info"].get("diseases", []),
            photo=values["photo"],
        )
        add_person_to_family(self.family.identifier, person)
        self.family = reload_family(self.family)
        self.set_active_family(self.family, preserve_positions=True)
        self.statusBar().showMessage(f"Added person: {person.name}")

    @Slot()
    def load_person_to_family(self) -> None:
        if self.family is None:
            QMessageBox.information(self, "No family", "Create or load a family first.")
            return
        self.refresh_people()
        labels = [label for label in available_people_labels(self.people_by_id) if person_id_from_label(label) not in self.family.members]
        if not labels:
            QMessageBox.information(self, "No available people", "Every saved person is already in this family, or no people exist.")
            return
        selected, ok = QInputDialog.getItem(self, "Load person to family", "Choose a person:", labels, 0, False)
        if not ok or not selected:
            return
        person_id = person_id_from_label(selected)
        person = self.people_by_id[person_id]
        add_person_to_family(self.family.identifier, person)
        self.family = reload_family(self.family)
        self.set_active_family(self.family, preserve_positions=True)
        self.statusBar().showMessage(f"Loaded {person.name} into {self.family.name}")

    @Slot(object)
    def edit_person(self, person: Person) -> None:
        dialog = PersonDialog(self, person)
        if dialog.exec() != QDialog.Accepted:
            return
        values = dialog.values()
        if not values["name"]:
            QMessageBox.warning(self, "Invalid person", "Name cannot be empty.")
            return
        upd_info_person(
            person.identifier,
            name=values["name"],
            birth=values["birth"],
            death=values["death"],
            health_info=values["health_info"],
            photo=values["photo"],
        )
        self.refresh_people()
        if self.family is not None:
            self.scene.set_family(self.family, self.people_by_id, preserve_positions=True)
            self.details.set_context(self.family, self.people_by_id)
        updated_person = self.people_by_id.get(person.identifier)
        self.details.set_person(updated_person)
        self.statusBar().showMessage(f"Updated person: {values['name']}")

    @Slot()
    def remove_selected_person_from_family(self) -> None:
        person = self.details.current_person
        if person is None:
            QMessageBox.information(self, "No person selected", "Select a person first.")
            return
        self.delete_person(person)

    @Slot()
    def remove_selected_person(self) -> None:
        person = self.details.current_person
        if person is None:
            QMessageBox.information(self, "No person selected", "Select a person first.")
            return
        self.remove_person(person)

    @Slot(object)
    def remove_person(self, person: Person) -> None:
        answer = QMessageBox.question(self, "Remove person", f"Remove {person.name} from the global people database and from every family?\n\nThis cannot be undone.")
        if answer != QMessageBox.Yes:
            return
        current_family_id = self.family.identifier if self.family is not None else None
        rmv_person(person.identifier)
        self.refresh_people()
        if current_family_id and find_family_file(current_family_id):
            self.family = load_family_by_id(current_family_id)
            self.set_active_family(self.family, preserve_positions=True)
        else:
            self.initial_screen()
        self.details.set_person(None)
        self.initial_page.refresh_families()
        self.statusBar().showMessage(f"Removed person: {person.name}")

    @Slot(object)
    def delete_person(self, person: Person) -> None:
        if self.family is None:
            return
        answer = QMessageBox.question(self, "Remove person from family", f"Remove {person.name} from {self.family.name}?\n\nThe person remains in the global people database.")
        if answer != QMessageBox.Yes:
            return
        remove_person_from_current_family(self.family, person.identifier)
        self.family = reload_family(self.family)
        self.set_active_family(self.family, preserve_positions=True)
        self.details.set_person(None)
        self.statusBar().showMessage(f"Removed person from family: {person.name}")

    @Slot(str)
    def start_two_click_connection(self, mode: str) -> None:
        if self.family is None:
            QMessageBox.information(self, "No family", "Create or load a family first.")
            self.show_initial_page()
            return
        self.show_graph_page()
        self.scene.start_connection_mode(mode)
        self.view.setFocus()

    @Slot(str)
    def show_person_disease_tracking(self, person_id: str) -> None:
        person = self.people_by_id.get(person_id)
        if person is None:
            return
        report = format_person_disease_tracking(person_id, self.people_by_id)
        self.details.display_report(f"Disease tracking: {person.name}", report, current_person=person)
        self.statusBar().showMessage(f"Showing disease tracking for: {person.name}")

    @Slot()
    def show_family_blood_types(self) -> None:
        if self.family is None:
            QMessageBox.information(self, "No family", "Create or load a family first.")
            return
        self.details.display_report(f"Blood types: {self.family.name}", format_family_blood_types_report(self.family.identifier))

    @Slot()
    def show_family_diseases(self) -> None:
        if self.family is None:
            QMessageBox.information(self, "No family", "Create or load a family first.")
            return
        self.details.display_report(f"Diseases: {self.family.name}", format_family_diseases_report(self.family.identifier))


# ---------------------------------------------------------------------
# Application entry point
# ---------------------------------------------------------------------


def main() -> None:
    app = QApplication(sys.argv)
    window = MainWindow()
    window.show()
    sys.exit(app.exec())


if __name__ == "__main__":
    main()
