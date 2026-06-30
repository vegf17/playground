import json
import os
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
    QListWidget,
    QListWidgetItem,
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
    add_disease,
    disease_ids_to_names,
    fill_new_dict,
    init_count_file,
    init_family,
    list_diseases,
    rmv_person,
    rmv_person_family,
    start,
    normalize_disease_refs,
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
    GRAPH_PADDING = 80
    PERSON_X_SPACING = 190
    PERSON_Y_SPACING = 170
    PAN_STEP = 45
    ZOOM_FACTOR = 1.15
    DETAILS_MIN_WIDTH = 310
    DETAILS_MAX_WIDTH = 390
    WINDOW_WIDTH = 1150
    WINDOW_HEIGHT = 740


TOOLBAR_LAYOUT = [
    ["Initial screen"],
    ["Load family", "Add family"],
    ["Add person", "Load person to family", "Rmv person", "Rmv person from family"],
    ["Family blood types", "Family diseases"],
    ["Connect partners", "Connect father to child", "Connect mother to child"],
    ["Rmv partners", "Rmv father-child", "Rmv mother-child"],
]



# ---------------------------------------------------------------------
# Backend/file helpers for the new classes.py/backend.py model
# ---------------------------------------------------------------------


def ensure_storage() -> None:
    """Create the data folders/count files expected by backend.py."""
    start()
    init_count_file()


def people_file_path() -> Path:
    return Path(DATA_SOURCE) / PPL_DIR / PPL_FILE


def family_dir_path() -> Path:
    return Path(DATA_SOURCE) / FAM_DIR


def family_id_from_stem(stem: str) -> str:
    """Extract 'f0' from stems like 'f0-smith_family'."""
    return stem.split("-", 1)[0]


def find_family_file(family_id: str) -> Optional[Path]:
    fam_dir = family_dir_path()
    if not fam_dir.exists():
        return None

    matches = sorted(fam_dir.glob(f"{family_id}-*.json"))
    return matches[0] if matches else None


def available_family_names() -> list[str]:
    fam_dir = family_dir_path()
    if not fam_dir.exists():
        return []
    return sorted(path.stem for path in fam_dir.glob("*.json"))


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
    file_path = find_family_file(family.identifier)
    if file_path is None:
        return family
    return load_family_by_stem(file_path.stem)


def save_family(family: Family) -> None:
    file_path = find_family_file(family.identifier)
    if file_path is None:
        file_path = family_dir_path() / f"{family.identifier}-{family.name.replace(' ', '_').lower()}.json"

    with open(file_path, "w", encoding="utf-8") as f:
        json.dump(family_to_json(family), f, indent=4, ensure_ascii=False)


def normalize_and_save_family(family: Family) -> None:
    # Guarantee every member has a relation dictionary before asking backend.py
    # to recompute derived relations.
    for person_id in list(family.members):
        family.relations.setdefault(person_id, fill_new_dict())

    family.relations = upd_family_relations(family.members, family.relations)
    save_family(family)


def person_label(people_by_id: dict[str, Person], person_id: Optional[str]) -> str:
    if not person_id:
        return "None"
    person = people_by_id.get(person_id)
    if person is None:
        return person_id
    return f"{person.name} ({person.identifier})"


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


def are_partners(family: Family, p1_id: str, p2_id: str) -> bool:
    return p2_id in relation_list(family, p1_id, "partners") and p1_id in relation_list(family, p2_id, "partners")


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
    if not parent_id or parent_id not in family.relations:
        return
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


def set_siblings(family: Family, p1_id: str, p2_id: str) -> None:
    if p1_id == p2_id:
        return

    p1_siblings = relation_list(family, p1_id, "siblings")
    p2_siblings = relation_list(family, p2_id, "siblings")

    if p2_id not in p1_siblings:
        p1_siblings.append(p2_id)
    if p1_id not in p2_siblings:
        p2_siblings.append(p1_id)

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
    # backend.py already removes the person from the family file and cleans the
    # family relations. It does not currently remove the family id from ppl.json,
    # so the frontend performs that small cleanup afterwards.
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
    labels = {
        "partner": "connect partners",
        "father": "connect father to child",
        "mother": "connect mother to child",
        "remove_partner": "remove partner connection",
        "remove_father": "remove father-child connection",
        "remove_mother": "remove mother-child connection",
        "sibling": "connect siblings",
    }
    return labels.get(mode, mode)



# ---------------------------------------------------------------------
# Health/medical report formatting helpers
# ---------------------------------------------------------------------


def format_disease_values(diseases) -> str:
    disease_names = disease_ids_to_names(diseases)
    return ", ".join(disease_names) if disease_names else "None"


def format_people_entries(entries: list[tuple[str, str]]) -> str:
    if not entries:
        return "None"
    return ", ".join(f"{name} ({person_id})" for name, person_id in entries)


def format_person_disease_tracking(person_id: str, people_by_id: dict[str, Person]) -> str:
    tracking = track_person_diseases(person_id)
    if not tracking:
        return "No tracked relative diseases found."

    lines = []
    for relative_id, info in tracking.items():
        person = people_by_id.get(relative_id)
        relative_name = person.name if person is not None else relative_id
        diseases = format_disease_values(info.get("disease", []))
        lines.append(
            f"{relative_name} ({relative_id})\n"
            f"  Family: {info.get('fam', 'Unknown')}\n"
            f"  Relation: {info.get('relation', 'Unknown')}\n"
            f"  Degree: {info.get('degree', 'Unknown')}\n"
            f"  Diseases: {diseases}"
        )
    return "\n\n".join(lines)


def format_family_blood_types_report(family_id: str) -> str:
    blood_types = fam_blood_types(family_id)
    if not blood_types:
        return "No blood type information found for this family."

    lines = []
    for blood_type, entries in blood_types.items():
        label = blood_type if blood_type else "Unknown"
        lines.append(f"{label}: {format_people_entries(entries)}")
    return "\n".join(lines)


def format_family_diseases_report(family_id: str) -> str:
    diseases = fam_diseases(family_id)
    if not diseases:
        return "No diseases found for this family."

    lines = []
    for disease, entries in diseases.items():
        label = disease if disease else "Unknown"
        lines.append(f"{label}: {format_people_entries(entries)}")
    return "\n".join(lines)


# ---------------------------------------------------------------------
# Graphics edge / intermediate union node
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
    RADIUS = UnionNodeStyle.RADIUS

    def __init__(self, p1_id: str, p2_id: str):
        super().__init__()
        self.p1_id = p1_id
        self.p2_id = p2_id
        self.connected_edges: list[EdgeItem] = []
        self.setFlags(
            QGraphicsItem.ItemIsMovable
            | QGraphicsItem.ItemIsSelectable
            | QGraphicsItem.ItemSendsGeometryChanges
        )

    def add_edge(self, edge: EdgeItem) -> None:
        self.connected_edges.append(edge)

    def itemChange(self, change, value):
        result = super().itemChange(change, value)
        if change == QGraphicsItem.ItemPositionHasChanged:
            for edge in list(self.connected_edges):
                edge.update_position()
        return result

    def boundingRect(self) -> QRectF:
        r = self.RADIUS
        return QRectF(-r, -r, 2 * r, 2 * r)

    def paint(self, painter: QPainter, option, widget=None) -> None:
        painter.setRenderHint(QPainter.Antialiasing)
        color = QColor(UIColors.SELECTED) if self.isSelected() else QColor(UIColors.PRIMARY)
        painter.setBrush(QBrush(color))
        painter.setPen(QPen(color, 1))
        painter.drawEllipse(self.boundingRect())


# ---------------------------------------------------------------------
# Graphics item representing one backend Person
# ---------------------------------------------------------------------


class PersonItem(QGraphicsObject):
    selected_person = Signal(str)
    hovered_person = Signal(str)
    right_clicked_person = Signal(str, object)

    WIDTH = PersonNodeStyle.WIDTH
    HEIGHT = PersonNodeStyle.HEIGHT

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

        self.setFlags(
            QGraphicsItem.ItemIsMovable
            | QGraphicsItem.ItemIsSelectable
            | QGraphicsItem.ItemSendsGeometryChanges
        )
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
        return QRectF(0, 0, self.WIDTH, self.HEIGHT)

    def paint(self, painter: QPainter, option, widget=None) -> None:
        rect = self.boundingRect()
        if self.is_hovered:
            border = QColor(UIColors.HOVER_BORDER)
        elif self.isSelected():
            border = QColor(UIColors.SELECTED)
        else:
            border = QColor(UIColors.PRIMARY)

        if self.isSelected():
            fill = QColor(UIColors.SELECTED_BACKGROUND)
        elif self.is_hovered:
            fill = QColor(UIColors.HOVER_BACKGROUND)
        else:
            fill = QColor(UIColors.PANEL_BACKGROUND)

        painter.setRenderHint(QPainter.Antialiasing)
        painter.setBrush(QBrush(fill))
        painter.setPen(QPen(border, PersonNodeStyle.BORDER_WIDTH))
        painter.drawEllipse(rect.adjusted(1, 1, -1, -1))

        if self.photo_pixmap is not None:
            ellipse_path = QPainterPath()
            ellipse_path.addEllipse(rect.adjusted(PersonNodeStyle.PHOTO_INSET, PersonNodeStyle.PHOTO_INSET, -PersonNodeStyle.PHOTO_INSET, -PersonNodeStyle.PHOTO_INSET))
            painter.save()
            painter.setClipPath(ellipse_path)
            scaled = self.photo_pixmap.scaled(
                int(rect.width()),
                int(rect.height()),
                Qt.KeepAspectRatioByExpanding,
                Qt.SmoothTransformation,
            )
            x = int(rect.x() + (rect.width() - scaled.width()) / 2)
            y = int(rect.y() + (rect.height() - scaled.height()) / 2)
            painter.drawPixmap(x, y, scaled)
            painter.restore()

            painter.setBrush(QBrush(QColor(*UIColors.PHOTO_LABEL_OVERLAY)))
            painter.setPen(Qt.NoPen)
            label_rect = QRectF(PersonNodeStyle.PHOTO_LABEL_MARGIN, self.HEIGHT - PersonNodeStyle.PHOTO_LABEL_BOTTOM_MARGIN, self.WIDTH - 2 * PersonNodeStyle.PHOTO_LABEL_MARGIN, PersonNodeStyle.PHOTO_LABEL_HEIGHT)
            painter.drawRoundedRect(label_rect, 8, 8)
            painter.setPen(QPen(QColor(UIColors.TEXT_SOFT)))
            painter.setFont(QFont(PersonNodeStyle.FONT_NAME, PersonNodeStyle.ID_FONT_SIZE, QFont.Bold))
            painter.drawText(label_rect, Qt.AlignCenter, self.person.name)
            self.draw_hover_highlight(painter, rect)
            return

        painter.setPen(QPen(QColor(UIColors.TEXT_SOFT)))
        painter.setFont(QFont(PersonNodeStyle.FONT_NAME, PersonNodeStyle.NAME_FONT_SIZE, QFont.Bold))
        painter.drawText(rect.adjusted(12, 18, -12, -45), Qt.AlignCenter, self.person.name or "Unnamed")
        painter.setFont(QFont(PersonNodeStyle.FONT_NAME, PersonNodeStyle.ID_FONT_SIZE))
        painter.drawText(rect.adjusted(12, 45, -12, -18), Qt.AlignCenter, self.person.identifier)

        self.draw_hover_highlight(painter, rect)

    def draw_hover_highlight(self, painter: QPainter, rect: QRectF) -> None:
        if not self.is_hovered:
            return

        painter.save()
        painter.setBrush(Qt.NoBrush)

        glow_pen = QPen(QColor(UIColors.HOVER_GLOW), PersonNodeStyle.BORDER_WIDTH + 5)
        glow_pen.setCosmetic(True)
        painter.setPen(glow_pen)
        painter.drawEllipse(rect.adjusted(4, 4, -4, -4))

        border_pen = QPen(QColor(UIColors.HOVER_BORDER), PersonNodeStyle.BORDER_WIDTH + 2)
        border_pen.setCosmetic(True)
        painter.setPen(border_pen)
        painter.drawEllipse(rect.adjusted(5, 5, -5, -5))

        painter.restore()

    def hoverEnterEvent(self, event) -> None:
        # Hover events already drive the details panel.  For the visual
        # highlight, reuse Qt's selected state because the selected-person
        # highlight is known to work in this GUI.  We remember whether the
        # item was selected before hover so leaving the node does not remove
        # a real user selection.
        self.is_hovered = True
        self.was_selected_before_hover = self.isSelected()
        self.setSelected(True)
        self.setZValue(20)
        self.update()
        self.hovered_person.emit(self.person.identifier)
        super().hoverEnterEvent(event)

    def hoverLeaveEvent(self, event) -> None:
        self.is_hovered = False
        if not self.was_selected_before_hover:
            self.setSelected(False)
        self.setZValue(self.normal_z_value)
        self.update()
        super().hoverLeaveEvent(event)

    def mousePressEvent(self, event) -> None:
        if event.button() == Qt.LeftButton:
            self.selected_person.emit(self.person.identifier)
        super().mousePressEvent(event)

    def contextMenuEvent(self, event) -> None:
        self.right_clicked_person.emit(self.person.identifier, event.screenPos())


# ---------------------------------------------------------------------
# Scene containing the family tree
# ---------------------------------------------------------------------


class FamilyTreeScene(QGraphicsScene):
    person_selected = Signal(object)
    status_message = Signal(str)
    family_changed = Signal(object)
    person_disease_tracking_requested = Signal(str)

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
            text = self.addText(
                f"Family: {self.family.name}\n\nNo members yet.\nUse 'Add person'.",
                QFont(PersonNodeStyle.FONT_NAME, 16),
            )
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
        roots = [
            pid for pid in member_ids
            if not get_relation(self.family, pid).get("father") and not get_relation(self.family, pid).get("mother")
        ]
        if not roots:
            roots = member_ids[:]

        for pid in roots:
            levels[pid] = 0

        for _ in range(max(4, len(member_ids) * 2)):
            changed = False
            for pid in member_ids:
                rel = get_relation(self.family, pid)
                parent_levels = []
                father = rel.get("father")
                mother = rel.get("mother")
                if father in member_ids:
                    parent_levels.append(levels.get(father, 0))
                if mother in member_ids:
                    parent_levels.append(levels.get(mother, 0))
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
        x_spacing = LayoutConfig.PERSON_X_SPACING
        y_spacing = LayoutConfig.PERSON_Y_SPACING

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
            start_x = -((total_slots - 1) * x_spacing) / 2
            slot = 0
            for group in ordered_groups:
                for pid in group:
                    layout[pid] = (start_x + slot * x_spacing, level * y_spacing)
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
            item.hovered_person.connect(self.handle_person_hovered)
            item.right_clicked_person.connect(self.open_person_menu)

            if pid in self.saved_person_positions:
                item.setPos(self.saved_person_positions[pid])
            else:
                x, y = layout.get(pid, (index * 190, 0))
                item.setPos(x, y)

            self.addItem(item)
            self.person_items[pid] = item

    def add_edge_between(self, source_item, target_item, pen: QPen) -> None:
        edge = EdgeItem(source_item, target_item, pen)
        self.addItem(edge)
        self.edge_items.append(edge)

    def get_person_item(self, person_id: Optional[str]) -> Optional[PersonItem]:
        if person_id is None:
            return None
        return self.person_items.get(person_id)

    def get_or_create_union_item(self, p1_id: str, p2_id: str) -> Optional[UnionItem]:
        p1_item = self.get_person_item(p1_id)
        p2_item = self.get_person_item(p2_id)
        if p1_item is None or p2_item is None:
            return None

        key = couple_key(p1_id, p2_id)
        existing = self.union_items.get(key)
        if existing is not None:
            return existing

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

            if father is not None and mother is not None:
                union_item = self.draw_partner_union(father, mother, partner_pen, drawn_partner_edges)
                if union_item is not None:
                    child_key = (couple_key(father, mother), child_id)
                    if child_key not in drawn_children:
                        self.add_edge_between(union_item, child_item, parent_pen)
                        drawn_children.add(child_key)
                continue

            if father is not None:
                father_item = self.get_person_item(father)
                if father_item is not None:
                    self.add_edge_between(father_item, child_item, parent_pen)

            if mother is not None:
                mother_item = self.get_person_item(mother)
                if mother_item is not None:
                    self.add_edge_between(mother_item, child_item, parent_pen)

    @Slot(str)
    def handle_person_hovered(self, person_id: str) -> None:
        person = self.people_by_id.get(person_id)
        self.person_selected.emit(person)

    @Slot(str)
    def handle_person_clicked(self, person_id: str) -> None:
        if self.family is None:
            return

        person = self.people_by_id.get(person_id)
        if self.mode is not None:
            if self.source_person_id is None:
                self.source_person_id = person_id
                self.person_selected.emit(person)
                self.status_message.emit(
                    f"{person.name if person else person_id} selected for {connection_mode_label(self.mode)}. "
                    "Now click a different second person."
                )
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

        # A right-click is now reserved for the medical/disease tracking report.
        # Editing/removal/connection actions remain available from the toolbar.
        self.person_selected.emit(person)
        self.person_disease_tracking_requested.emit(person_id)

    def start_connection_mode(self, mode: str, source: Optional[str] = None) -> None:
        self.mode = mode
        self.source_person_id = source

        if source is None:
            self.status_message.emit(
                f"Connection mode: {connection_mode_label(mode)}. "
                "Click the first person, then click the second person. Press Esc to cancel."
            )
            self.person_selected.emit(None)
            return

        person = self.people_by_id.get(source)
        self.status_message.emit(
            f"Connection mode: {connection_mode_label(mode)}. "
            f"Source: {person.name if person else source}. Now click another person. Press Esc to cancel."
        )
        self.person_selected.emit(person)

    def cancel_connection_mode(self) -> None:
        if self.mode is None:
            return
        self.mode = None
        self.source_person_id = None
        self.status_message.emit("Connection mode cancelled.")

    def apply_connection(self, source_id: str, target_id: str) -> None:
        if self.family is None:
            return

        source_name = self.people_by_id[source_id].name if source_id in self.people_by_id else source_id
        target_name = self.people_by_id[target_id].name if target_id in self.people_by_id else target_id

        if self.mode == "partner":
            already = are_partners(self.family, source_id, target_id)
            set_partner(self.family, source_id, target_id)
            self.status_message.emit(
                f"{source_name} and {target_name} were already partners."
                if already else f"{source_name} and {target_name} are now partners."
            )
        elif self.mode == "father":
            set_father(self.family, target_id, source_id)
            self.status_message.emit(f"{source_name} is now father of {target_name}.")
        elif self.mode == "mother":
            set_mother(self.family, target_id, source_id)
            self.status_message.emit(f"{source_name} is now mother of {target_name}.")
        elif self.mode == "remove_partner":
            if remove_partner_connection(self.family, source_id, target_id):
                self.status_message.emit(f"Removed partner connection between {source_name} and {target_name}.")
            else:
                self.status_message.emit(f"No partner connection exists between {source_name} and {target_name}.")
        elif self.mode == "remove_father":
            if remove_parent_child_connection(self.family, source_id, target_id, "father"):
                self.status_message.emit(f"Removed father-child connection between {source_name} and {target_name}.")
            else:
                self.status_message.emit(f"No father-child connection exists between {source_name} and {target_name}.")
        elif self.mode == "remove_mother":
            if remove_parent_child_connection(self.family, source_id, target_id, "mother"):
                self.status_message.emit(f"Removed mother-child connection between {source_name} and {target_name}.")
            else:
                self.status_message.emit(f"No mother-child connection exists between {source_name} and {target_name}.")
        elif self.mode == "sibling":
            set_siblings(self.family, source_id, target_id)
            self.status_message.emit(f"{source_name} and {target_name} are now siblings.")


# ---------------------------------------------------------------------
# View
# ---------------------------------------------------------------------


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
        if not isinstance(scene, FamilyTreeScene):
            return
        rect = scene.family_bounding_rect()
        if not rect.isNull():
            self.fitInView(rect.adjusted(-LayoutConfig.GRAPH_PADDING, -LayoutConfig.GRAPH_PADDING, LayoutConfig.GRAPH_PADDING, LayoutConfig.GRAPH_PADDING), Qt.KeepAspectRatio)

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
# Initial page
# ---------------------------------------------------------------------


class InitialPage(QWidget):
    add_family_requested = Signal()
    family_selected = Signal(str)

    def __init__(self):
        super().__init__()
        self.title = QLabel("FamilyTree")
        self.title.setAlignment(Qt.AlignCenter)
        self.title.setStyleSheet("font-size: 28px; font-weight: bold; color: #0044ff;")
        self.subtitle = QLabel("Select an existing family or add a new one.")
        self.subtitle.setAlignment(Qt.AlignCenter)
        self.subtitle.setStyleSheet("color: #0044ff;")
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
        box.addWidget(QLabel("Select family"))
        box.addWidget(self.family_combo)
        box.addLayout(button_row)

        outer = QVBoxLayout(self)
        outer.addStretch()
        outer.addLayout(box)
        outer.addStretch()
        self.refresh_families()
        self.setStyleSheet("""
            QWidget { background-color: #000000; }
            QLabel { color: #0044ff; }
            QComboBox { color: #0044ff; background-color: #000000; border: 1px solid #0044ff; padding: 6px; }
            QComboBox QAbstractItemView { color: #0044ff; background-color: #050510; border: 1px solid #0044ff; selection-background-color: #101040; }
            QPushButton { color: #0044ff; background-color: #000000; border: 1px solid #0044ff; padding: 6px; }
            QPushButton:hover { background-color: #101040; }
        """)

    def refresh_families(self) -> None:
        self.family_combo.clear()
        self.family_combo.addItem("Select family", "")
        for name in available_family_names():
            self.family_combo.addItem(name, name)

    def emit_selected_family(self) -> None:
        selected = self.family_combo.currentData()
        if selected:
            self.family_selected.emit(selected)


# ---------------------------------------------------------------------
# Dialog for adding/editing backend Person objects
# ---------------------------------------------------------------------


class PersonDialog(QDialog):
    def __init__(self, parent=None, person: Optional[Person] = None):
        super().__init__(parent)
        self.setWindowTitle("Person information")
        self.selected_disease_ids: list[str] = []

        self.name_input = QLineEdit()
        self.birth_input = QLineEdit()
        self.death_input = QLineEdit()
        self.blood_type_input = QLineEdit()
        self.clinical_history_input = QLineEdit()
        self.photo_input = QLineEdit()
        self.clinical_history_input.setPlaceholderText("simple text for now")

        self.disease_combo = QComboBox()
        self.add_existing_disease_button = QPushButton("Add selected")
        self.new_disease_input = QLineEdit()
        self.new_disease_input.setPlaceholderText("type a disease not in the list")
        self.create_disease_button = QPushButton("Create disease")
        self.selected_diseases_list = QListWidget()
        self.selected_diseases_list.setMinimumHeight(90)
        self.remove_selected_disease_button = QPushButton("Remove selected disease")

        self.add_existing_disease_button.clicked.connect(self.add_selected_disease)
        self.create_disease_button.clicked.connect(self.create_disease_from_text)
        self.remove_selected_disease_button.clicked.connect(self.remove_selected_disease)

        self.refresh_disease_combo()

        existing_disease_row = QHBoxLayout()
        existing_disease_row.addWidget(self.disease_combo)
        existing_disease_row.addWidget(self.add_existing_disease_button)

        new_disease_row = QHBoxLayout()
        new_disease_row.addWidget(self.new_disease_input)
        new_disease_row.addWidget(self.create_disease_button)

        selected_disease_box = QVBoxLayout()
        selected_disease_box.addWidget(self.selected_diseases_list)
        selected_disease_box.addWidget(self.remove_selected_disease_button)

        browse_photo = QPushButton("Browse")
        remove_photo = QPushButton("Remove")
        browse_photo.clicked.connect(self.browse_photo)
        remove_photo.clicked.connect(lambda: self.photo_input.setText(""))
        photo_row = QHBoxLayout()
        photo_row.addWidget(self.photo_input)
        photo_row.addWidget(browse_photo)
        photo_row.addWidget(remove_photo)

        if person is not None:
            self.name_input.setText(person.name)
            self.birth_input.setText(person.birth)
            self.death_input.setText(person.death)
            self.blood_type_input.setText(person.health_info.get("blood_type", ""))
            self.selected_disease_ids = normalize_disease_refs(person.health_info.get("diseases", []))
            clinical_history = person.health_info.get("clinical_history", [])
            self.clinical_history_input.setText("; ".join(clinical_history) if isinstance(clinical_history, list) else str(clinical_history))
            self.photo_input.setText(person.photo)

        self.update_selected_diseases_widget()

        form = QFormLayout()
        form.addRow("Name:", self.name_input)
        form.addRow("Birth:", self.birth_input)
        form.addRow("Death:", self.death_input)
        form.addRow("Blood type:", self.blood_type_input)
        form.addRow("Existing disease:", existing_disease_row)
        form.addRow("New disease:", new_disease_row)
        form.addRow("Selected diseases:", selected_disease_box)
        form.addRow("Clinical history:", self.clinical_history_input)
        form.addRow("Photo:", photo_row)

        ok_button = QPushButton("OK")
        cancel_button = QPushButton("Cancel")
        ok_button.clicked.connect(self.accept)
        cancel_button.clicked.connect(self.reject)
        buttons = QHBoxLayout()
        buttons.addWidget(ok_button)
        buttons.addWidget(cancel_button)

        layout = QVBoxLayout(self)
        layout.addLayout(form)
        layout.addLayout(buttons)

    def refresh_disease_combo(self, selected_disease_id: Optional[str] = None) -> None:
        self.disease_combo.clear()
        self.disease_combo.addItem("Select existing disease", "")
        selected_index = 0
        for index, (disease_id, disease_name) in enumerate(list_diseases(), start=1):
            self.disease_combo.addItem(f"{disease_name} ({disease_id})", disease_id)
            if disease_id == selected_disease_id:
                selected_index = index
        self.disease_combo.setCurrentIndex(selected_index)

    def update_selected_diseases_widget(self) -> None:
        self.selected_diseases_list.clear()
        disease_names = disease_ids_to_names(self.selected_disease_ids)
        for disease_id, disease_name in zip(self.selected_disease_ids, disease_names):
            item = QListWidgetItem(f"{disease_name} ({disease_id})")
            item.setData(Qt.UserRole, disease_id)
            self.selected_diseases_list.addItem(item)

    def add_disease_to_selection(self, disease_id: Optional[str]) -> None:
        if not disease_id:
            return
        if disease_id not in self.selected_disease_ids:
            self.selected_disease_ids.append(disease_id)
            self.update_selected_diseases_widget()

    def add_selected_disease(self) -> None:
        self.add_disease_to_selection(self.disease_combo.currentData())

    def create_disease_from_text(self) -> None:
        disease_name = self.new_disease_input.text().strip()
        if not disease_name:
            QMessageBox.information(self, "No disease name", "Type a disease name first.")
            return
        try:
            disease_id = add_disease(disease_name)
        except ValueError as error:
            QMessageBox.warning(self, "Invalid disease", str(error))
            return

        self.new_disease_input.clear()
        self.refresh_disease_combo(disease_id)
        self.add_disease_to_selection(disease_id)

    def remove_selected_disease(self) -> None:
        item = self.selected_diseases_list.currentItem()
        if item is None:
            return
        disease_id = item.data(Qt.UserRole)
        if disease_id in self.selected_disease_ids:
            self.selected_disease_ids.remove(disease_id)
            self.update_selected_diseases_widget()

    def browse_photo(self) -> None:
        filename, _ = QFileDialog.getOpenFileName(
            self,
            "Select photo",
            "",
            "Images (*.png *.jpg *.jpeg *.bmp *.gif *.webp);;All files (*)",
        )
        if filename:
            self.photo_input.setText(filename)

    def values(self) -> dict:
        clinical_text = self.clinical_history_input.text().strip()
        clinical_history = [clinical_text] if clinical_text else []
        return {
            "name": self.name_input.text().strip(),
            "birth": self.birth_input.text().strip(),
            "death": self.death_input.text().strip(),
            "health_info": {
                "blood_type": self.blood_type_input.text().strip(),
                "diseases": list(self.selected_disease_ids),
                "clinical_history": clinical_history,
            },
            "photo": self.photo_input.text().strip(),
        }


# ---------------------------------------------------------------------
# Right-side details panel
# ---------------------------------------------------------------------


class DetailsPanel(QFrame):
    add_person_requested = Signal()
    edit_person_requested = Signal(object)
    remove_from_family_requested = Signal(object)
    remove_person_requested = Signal(object)
    connection_requested = Signal(str)

    def __init__(self):
        super().__init__()
        self.current_person: Optional[Person] = None
        self.family: Optional[Family] = None
        self.people_by_id: dict[str, Person] = {}
        self.setFrameShape(QFrame.StyledPanel)
        self.setMinimumWidth(LayoutConfig.DETAILS_MIN_WIDTH)
        self.setMaximumWidth(LayoutConfig.DETAILS_MAX_WIDTH)

        self.title = QLabel("No person selected")
        self.title.setObjectName("detailsTitle")
        self.info = QLabel("Move the mouse over a person node, select one, or right-click one.")
        self.info.setObjectName("detailsInfo")
        self.info.setWordWrap(True)

        self.edit_person_button = QPushButton("Edit person")
        self.remove_from_family_button = QPushButton("Remove from the current family")
        self.remove_person_button = QPushButton("Remove person")

        self.edit_person_button.clicked.connect(self.emit_edit)
        self.remove_from_family_button.clicked.connect(self.emit_remove_from_family)
        self.remove_person_button.clicked.connect(self.emit_remove_person)

        self.person_actions = QWidget()
        person_actions_layout = QVBoxLayout(self.person_actions)
        person_actions_layout.setContentsMargins(0, 8, 0, 0)
        person_actions_layout.addWidget(self.edit_person_button)
        person_actions_layout.addWidget(self.remove_from_family_button)
        person_actions_layout.addWidget(self.remove_person_button)
        self.person_actions.hide()

        layout = QVBoxLayout(self)
        layout.addWidget(self.title)
        layout.addWidget(self.info)
        layout.addWidget(self.person_actions)
        layout.addStretch()



    def set_context(self, family: Optional[Family], people_by_id: dict[str, Person]) -> None:
        self.family = family
        self.people_by_id = people_by_id
        self.set_person(self.current_person if self.current_person else None)

    def set_person(self, person: Optional[Person]) -> None:
        self.current_person = person
        if person is None:
            self.title.setText("No person selected")
            self.info.setText("Move the mouse over a person node, select one, or right-click one.")
            self.person_actions.hide()
            return

        self.person_actions.show()

        rel = get_relation(self.family, person.identifier) if self.family is not None else fill_new_dict()
        health = person.health_info or {}
        diseases = disease_ids_to_names(health.get("diseases", []))
        clinical_history = health.get("clinical_history", [])
        families = person.families if person.families else []

        self.title.setText(person.name)
        self.info.setText(
            f"ID: {person.identifier}\n"
            f"Families: {', '.join(families) if families else 'None'}\n\n"
            #f"Photo: {person.photo or 'None'}\n\n"
            f"Personal information\n"
            f"Birth: {person.birth or 'Unknown'}\n"
            f"Death: {person.death or 'Not yet'}\n"
            f"Blood type: {health.get('blood_type', '') or 'Unknown'}\n"
            f"Diseases: {', '.join(diseases) if diseases else 'None'}\n\n"
            #f"Clinical history: {clinical_history if clinical_history else 'None'}\n\n"
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
        self.person_actions.hide()

    def emit_edit(self) -> None:
        if self.current_person is not None:
            self.edit_person_requested.emit(self.current_person)

    def emit_remove_from_family(self) -> None:
        if self.current_person is not None:
            self.remove_from_family_requested.emit(self.current_person)

    def emit_remove_person(self) -> None:
        if self.current_person is not None:
            self.remove_person_requested.emit(self.current_person)

    def emit_connection(self, mode: str) -> None:
        self.connection_requested.emit(mode)


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
        self.view.shortcuts_requested.connect(self.show_shortcuts)

        self.details.add_person_requested.connect(self.add_person)
        self.details.edit_person_requested.connect(self.edit_person)
        self.details.remove_from_family_requested.connect(self.delete_person)
        self.details.remove_person_requested.connect(self.remove_person_everywhere)
        self.details.connection_requested.connect(self.start_two_click_connection)

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
            QMenu::item:selected {{ background-color: {UIColors.SELECTED_BACKGROUND}; }}
            QDialog {{ background-color: {UIColors.PANEL_BACKGROUND}; color: {UIColors.PRIMARY}; }}
            QLabel {{ color: {UIColors.PRIMARY}; }}
            QLabel#detailsTitle {{ font-size: 18px; font-weight: bold; color: {UIColors.PRIMARY}; }}
            QLabel#detailsInfo {{ color: {UIColors.PRIMARY}; }}
            QLineEdit {{ color: {UIColors.PRIMARY}; background-color: {UIColors.BACKGROUND}; border: 1px solid {UIColors.PRIMARY}; padding: 4px; }}
            QComboBox {{ color: {UIColors.PRIMARY}; background-color: {UIColors.BACKGROUND}; border: 1px solid {UIColors.PRIMARY}; padding: 4px; }}
            QComboBox QAbstractItemView {{ color: {UIColors.PRIMARY}; background-color: {UIColors.PANEL_BACKGROUND}; border: 1px solid {UIColors.PRIMARY}; selection-background-color: {UIColors.SELECTED_BACKGROUND}; }}
            QListWidget {{ color: {UIColors.PRIMARY}; background-color: {UIColors.BACKGROUND}; border: 1px solid {UIColors.PRIMARY}; padding: 4px; }}
            QPushButton {{ color: {UIColors.PRIMARY}; background-color: {UIColors.BACKGROUND}; border: 1px solid {UIColors.PRIMARY}; padding: 6px; }}
            QPushButton:hover {{ background-color: {UIColors.SELECTED_BACKGROUND}; }}
            QInputDialog {{ background-color: {UIColors.PANEL_BACKGROUND}; }}
        """)

    def toolbar_action_map(self) -> dict[str, QAction]:
        actions = {
            "Initial screen": QAction("Initial screen", self),
            "Center graph": QAction("Center graph", self),
            "Add family": QAction("Add family", self),
            "Load family": QAction("Load family", self),
            "Add person": QAction("Add person", self),
            "Load person to family": QAction("Load person to family", self),
            "Rmv person from family": QAction("Rmv person from family", self),
            "Rmv person": QAction("Rmv person", self),
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
        actions["Center graph"].triggered.connect(self.view.center_family_graph)
        actions["Add family"].triggered.connect(self.create_family)
        actions["Load family"].triggered.connect(self.load_existing_family)
        actions["Add person"].triggered.connect(self.add_person)
        actions["Load person to family"].triggered.connect(self.load_person_to_family)
        actions["Rmv person from family"].triggered.connect(self.remove_selected_person_from_family)
        actions["Rmv person"].triggered.connect(self.remove_selected_person)
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
        for group_index, group in enumerate(TOOLBAR_LAYOUT):
            if group_index > 0:
                toolbar.addSeparator()
            for action_name in group:
                toolbar.addAction(actions[action_name])

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
        self.family = family
        self.refresh_people()
        self.details.set_context(self.family, self.people_by_id)
        self.scene.people_by_id = self.people_by_id

    @Slot()
    def create_family(self) -> None:
        name, ok = QInputDialog.getText(self, "Create family", "Family name:")
        if not ok or not name.strip():
            return

        family = init_family(name.strip())
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
    def save_current_family(self) -> None:
        if self.family is None:
            QMessageBox.information(self, "No family", "Create or load a family first.")
            return
        normalize_and_save_family(self.family)
        self.statusBar().showMessage(f"Saved family: {self.family.name}")

    @Slot()
    def show_family(self) -> None:
        if self.family is None:
            self.show_initial_page()
            self.statusBar().showMessage("Select or create a family first.")
            return
        self.family = reload_family(self.family)
        self.refresh_people()
        self.scene.set_family(self.family, self.people_by_id)
        self.details.set_context(self.family, self.people_by_id)
        self.show_graph_page()
        self.view.center_family_graph()
        self.statusBar().showMessage(f"Showing family: {self.family.name}")

    @Slot()
    def initial_screen(self) -> None:
        self.scene.clear_to_initial_screen()
        self.details.set_context(None, {})
        self.details.set_person(None)
        self.show_initial_page()
        self.statusBar().showMessage("Initial screen")

    @Slot()
    def load_person_to_family(self) -> None:
        if self.family is None:
            QMessageBox.information(self, "No family", "Create or load a family first.")
            self.show_initial_page()
            return

        self.refresh_people()
        available_people = {
            person_id: person
            for person_id, person in self.people_by_id.items()
            if person_id not in self.family.members
        }

        if not available_people:
            QMessageBox.information(
                self,
                "No available people",
                "All existing people are already in this family, or there are no people yet.",
            )
            return

        labels_by_text = {
            f"{person.name} ({person_id})": person
            for person_id, person in sorted(available_people.items(), key=lambda item: (item[1].name.lower(), item[0]))
        }

        selected, ok = QInputDialog.getItem(
            self,
            "Load person to family",
            "Choose an existing person:",
            list(labels_by_text.keys()),
            0,
            False,
        )

        if not ok or not selected:
            return

        person = labels_by_text[selected]
        add_person_to_family(self.family.identifier, person)
        self.family = reload_family(self.family)
        self.set_active_family(self.family, preserve_positions=True)
        self.details.set_person(load_all_people().get(person.identifier))
        self.statusBar().showMessage(f"Loaded person to family: {person.name}")

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
            "Hover person: show information\n"
            "Left-click person: select person / use in connection mode\n"
            "Right-click person: show disease tracking\n"
            "Mouse wheel: zoom graph",
        )

    @Slot()
    def add_person(self) -> None:
        if self.family is None:
            QMessageBox.information(self, "No family", "Create or load a family first.")
            self.show_initial_page()
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
            health_info=values["health_info"],
            photo=values["photo"],
        )
        add_person_to_family(self.family.identifier, person)
        self.family = reload_family(self.family)
        self.set_active_family(self.family, preserve_positions=True)
        self.details.set_person(load_all_people().get(person.identifier))
        self.statusBar().showMessage(f"Added person: {person.name}")

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
        updated_person = self.people_by_id.get(person.identifier)
        self.scene.set_family(self.family, self.people_by_id, preserve_positions=True)
        self.details.set_context(self.family, self.people_by_id)
        self.details.set_person(updated_person)
        self.statusBar().showMessage(f"Updated person: {values['name']}")

    @Slot(str)
    def show_person_disease_tracking(self, person_id: str) -> None:
        person = self.people_by_id.get(person_id)
        if person is None:
            return

        report = format_person_disease_tracking(person_id, self.people_by_id)
        self.details.display_report(
            f"Disease tracking: {person.name}",
            report,
            current_person=person,
        )
        self.statusBar().showMessage(f"Showing disease tracking for: {person.name}")

    @Slot()
    def show_family_blood_types(self) -> None:
        if self.family is None:
            QMessageBox.information(self, "No family", "Create or load a family first.")
            self.show_initial_page()
            return

        report = format_family_blood_types_report(self.family.identifier)
        self.details.display_report(f"Blood types: {self.family.name}", report)
        self.show_graph_page()
        self.statusBar().showMessage(f"Showing blood types for family: {self.family.name}")

    @Slot()
    def show_family_diseases(self) -> None:
        if self.family is None:
            QMessageBox.information(self, "No family", "Create or load a family first.")
            self.show_initial_page()
            return

        report = format_family_diseases_report(self.family.identifier)
        self.details.display_report(f"Diseases: {self.family.name}", report)
        self.show_graph_page()
        self.statusBar().showMessage(f"Showing diseases for family: {self.family.name}")

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

        self.remove_person_everywhere(person)

    @Slot(object)
    def remove_person_everywhere(self, person: Person) -> None:
        answer = QMessageBox.question(
            self,
            "Remove person",
            f"Remove {person.name} from the global people database and from every family?\n\n"
            "This cannot be undone.",
        )
        if answer != QMessageBox.Yes:
            return

        current_family_id = self.family.identifier if self.family is not None else None
        rmv_person(person.identifier)
        self.refresh_people()

        if current_family_id is not None:
            file_path = find_family_file(current_family_id)
            if file_path is not None:
                self.family = load_family_by_stem(file_path.stem)
                self.set_active_family(self.family, preserve_positions=True)
            else:
                self.family = None
                self.scene.clear_to_initial_screen()
                self.details.set_context(None, {})
                self.show_initial_page()
        else:
            self.details.set_context(None, self.people_by_id)

        self.details.set_person(None)
        self.initial_page.refresh_families()
        self.statusBar().showMessage(f"Removed person: {person.name}")

    @Slot(object)
    def delete_person(self, person: Person) -> None:
        if self.family is None:
            return
        answer = QMessageBox.question(
            self,
            "Remove person from family",
            f"Remove {person.name} from {self.family.name}?\n\nThe person remains in the global people database.",
        )
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
