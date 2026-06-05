import sys
from pathlib import Path
from typing import Optional

from PySide6.QtCore import Qt, QRectF, QPointF, Signal, Slot
from PySide6.QtGui import QAction, QBrush, QColor, QFont, QPainter, QPen
from PySide6.QtWidgets import (
    QApplication,
    QComboBox,
    QDialog,
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

from person import Person, Family
from fam import (
    DATA_SOURCE,
    init_family,
    save_family,
    load_family,
    add_new_member,
    upd_member_info,
    upd_family_relations,
    delete_member,
)


# ---------------------------------------------------------------------
# Small frontend helpers around your backend
# ---------------------------------------------------------------------

def person_label(person: Optional[Person]) -> str:
    if person is None:
        return "None"
    return f"{person.name} ({person.identifier})"


def get_person_id(person: Person) -> str:
    return person.identifier or f"unknown-{id(person)}"


def couple_key(p1: Person, p2: Person) -> tuple[str, str]:
    return tuple(sorted([get_person_id(p1), get_person_id(p2)]))


def list_names(people: list[Person]) -> str:
    return ", ".join(p.name for p in people) if people else "None"


def partners_of(person: Person) -> list[Person]:
    """Return the new multi-partner list, creating it if needed.

    This keeps the GUI robust if a partially migrated Person object is opened.
    """
    if not hasattr(person, "partners") or person.partners is None:
        person.partners = []
    return person.partners


def are_partners(p1: Person, p2: Person) -> bool:
    return p2 in partners_of(p1) and p1 in partners_of(p2)


def add_partner_link(p1: Person, p2: Person) -> bool:
    """Add a symmetric partner link. Return True if anything changed."""
    if p1 is p2:
        return False

    changed = False

    if p2 not in partners_of(p1):
        p1.partners.append(p2)
        changed = True

    if p1 not in partners_of(p2):
        p2.partners.append(p1)
        changed = True

    return changed


def remove_partner_link(p1: Person, p2: Person) -> bool:
    """Remove a symmetric partner link. Return True if anything changed."""
    changed = False

    if p2 in partners_of(p1):
        p1.partners.remove(p2)
        changed = True

    if p1 in partners_of(p2):
        p2.partners.remove(p1)
        changed = True

    return changed


def remove_child_from_parent(parent: Optional[Person], child: Person) -> None:
    if parent is not None and child in parent.kids:
        parent.kids.remove(child)


def available_family_names() -> list[str]:
    """Return saved family names from DATA_SOURCE.

    Families are stored as folders under DATA_SOURCE, each containing one JSON file.
    The returned names are the JSON stems, e.g. 'smith-f0'.
    """
    family_path = Path(DATA_SOURCE)
    if not family_path.exists():
        return []

    family_files = []
    for folder in family_path.iterdir():
        if folder.is_dir():
            family_files.extend(folder.glob("*.json"))

    return sorted(path.stem for path in family_files)


def family_id_from_stem(stem: str) -> str:
    """Extract the final family id from a saved family stem, e.g. 'smith-f0' -> 'f0'."""
    if "-" not in stem:
        return ""
    return stem.rsplit("-", 1)[1]


def set_partner(family: Family, p1: Person, p2: Person) -> None:
    """Connect two people as partners without removing existing partners."""
    if add_partner_link(p1, p2):
        upd_family_relations(family, p1)
        upd_family_relations(family, p2)
        save_family(family)


def set_father(family: Family, child: Person, father: Person) -> None:
    if child is father:
        return

    remove_child_from_parent(child.father, child)
    child.father = father

    if child not in father.kids:
        father.kids.append(child)

    # If both parents are known, represent that parental union as a partner link.
    if child.mother is not None and child.mother is not father:
        add_partner_link(father, child.mother)

    upd_family_relations(family, child)
    save_family(family)


def set_mother(family: Family, child: Person, mother: Person) -> None:
    if child is mother:
        return

    remove_child_from_parent(child.mother, child)
    child.mother = mother

    if child not in mother.kids:
        mother.kids.append(child)

    # If both parents are known, represent that parental union as a partner link.
    if child.father is not None and child.father is not mother:
        add_partner_link(child.father, mother)

    upd_family_relations(family, child)
    save_family(family)


def set_siblings(family: Family, p1: Person, p2: Person) -> None:
    if p1 is p2:
        return

    if p2 not in p1.siblings:
        p1.siblings.append(p2)

    if p1 not in p2.siblings:
        p2.siblings.append(p1)

    save_family(family)


def remove_partner_connection(family: Family, p1: Person, p2: Person) -> bool:
    """Remove a partner relation between p1 and p2, if it exists."""
    changed = remove_partner_link(p1, p2)

    if changed:
        save_family(family)

    return changed


def remove_father_child_connection(family: Family, p1: Person, p2: Person) -> bool:
    """Remove father-child relation between two people, regardless of click order."""
    changed = False

    if p2.father is p1:
        p2.father = None
        remove_child_from_parent(p1, p2)
        changed = True

    if p1.father is p2:
        p1.father = None
        remove_child_from_parent(p2, p1)
        changed = True

    if changed:
        save_family(family)

    return changed


def remove_mother_child_connection(family: Family, p1: Person, p2: Person) -> bool:
    """Remove mother-child relation between two people, regardless of click order."""
    changed = False

    if p2.mother is p1:
        p2.mother = None
        remove_child_from_parent(p1, p2)
        changed = True

    if p1.mother is p2:
        p1.mother = None
        remove_child_from_parent(p2, p1)
        changed = True

    if changed:
        save_family(family)

    return changed


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
        self.setLine(
            source_center.x(),
            source_center.y(),
            target_center.x(),
            target_center.y(),
        )


class UnionItem(QGraphicsObject):
    """Small intermediate node used to represent a couple/union.

    Partner nodes connect to this bullet point, and children connect from
    this bullet point instead of directly from each parent. The bullet is
    movable, so the user can adjust the shape of the tree manually.
    """

    RADIUS = 5

    def __init__(self, p1: Person, p2: Person):
        super().__init__()
        self.p1 = p1
        self.p2 = p2
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
        if self.isSelected():
            painter.setBrush(QBrush(QColor("#00ffff")))
            painter.setPen(QPen(QColor("#00ffff"), 1))
        else:
            painter.setBrush(QBrush(QColor("#0044ff")))
            painter.setPen(QPen(QColor("#0044ff"), 1))
        painter.drawEllipse(self.boundingRect())


# ---------------------------------------------------------------------
# Graphics item representing one backend Person
# ---------------------------------------------------------------------

class PersonItem(QGraphicsObject):
    selected_person = Signal(object)
    right_clicked_person = Signal(object, object)

    WIDTH = 130
    HEIGHT = 62

    def __init__(self, person: Person):
        super().__init__()

        self.person = person
        self.connected_edges: list[EdgeItem] = []

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

        if self.isSelected():
            border = QColor("#00ffff")
            fill = QColor("#101040")
        else:
            border = QColor("#0044ff")
            fill = QColor("#050510")

        painter.setRenderHint(QPainter.Antialiasing)

        painter.setBrush(QBrush(fill))
        painter.setPen(QPen(border, 2))
        painter.drawRoundedRect(rect, 8, 8)

        painter.setPen(QPen(QColor("#0066ff")))
        painter.setFont(QFont("Arial", 10, QFont.Bold))
        painter.drawText(
            rect.adjusted(8, 6, -8, -32),
            Qt.AlignLeft,
            self.person.name,
        )

        painter.setFont(QFont("Arial", 8))
        second_line = self.person.birth if self.person.birth else get_person_id(self.person)
        painter.drawText(
            rect.adjusted(8, 30, -8, -6),
            Qt.AlignLeft,
            second_line,
        )

    def mousePressEvent(self, event) -> None:
        if event.button() == Qt.LeftButton:
            self.selected_person.emit(self.person)

        super().mousePressEvent(event)

    def contextMenuEvent(self, event) -> None:
        self.right_clicked_person.emit(self.person, event.screenPos())


# ---------------------------------------------------------------------
# Scene containing the family tree
# ---------------------------------------------------------------------

class FamilyTreeScene(QGraphicsScene):
    person_selected = Signal(object)
    status_message = Signal(str)

    def __init__(self):
        super().__init__()

        self.setSceneRect(-2500, -2500, 5000, 5000)
        self.setBackgroundBrush(QBrush(QColor("#000000")))

        self.family: Optional[Family] = None
        self.person_items: dict[str, PersonItem] = {}
        self.union_items: dict[tuple[str, str], UnionItem] = {}
        self.edge_items: list[EdgeItem] = []

        self.mode: Optional[str] = None
        self.source_person: Optional[Person] = None

        # Positions are preserved across redraws so editing information, adding
        # members, or changing relations does not unexpectedly rearrange the graph.
        self.saved_person_positions: dict[str, QPointF] = {}
        self.saved_union_positions: dict[tuple[str, str], QPointF] = {}

    def remember_current_positions(self) -> None:
        for person_id, item in self.person_items.items():
            self.saved_person_positions[person_id] = item.pos()

        for key, item in self.union_items.items():
            self.saved_union_positions[key] = item.pos()

    def set_family(self, family: Family, preserve_positions: bool = True) -> None:
        if preserve_positions:
            self.remember_current_positions()
        self.family = family
        self.rebuild()

    def clear_to_initial_screen(self) -> None:
        self.family = None
        self.clear()
        self.person_items.clear()
        self.union_items.clear()
        self.edge_items.clear()
        self.saved_person_positions.clear()
        self.saved_union_positions.clear()
        self.draw_initial_screen()

    def draw_initial_screen(self) -> None:
        self.clear()

        title = self.addText("FamilyTree", QFont("Arial", 24, QFont.Bold))
        title.setDefaultTextColor(QColor("#0044ff"))
        title.setPos(-110, -130)

        hint = self.addText(
            "Select an existing family or add a new one.",
            QFont("Arial", 11),
        )
        hint.setDefaultTextColor(QColor("#0044ff"))
        hint.setPos(-190, -80)

    def family_bounding_rect(self) -> QRectF:
        """Bounding box of only the family graph items, not the whole scene."""
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

        if not self.family.fam:
            text = self.addText(
                f"Family: {self.family.fam_name}\n\n"
                "No members yet.\n"
                "Use 'Add person'.",
                QFont("Arial", 16),
            )
            text.setDefaultTextColor(QColor("#0044ff"))
            text.setPos(-160, -100)
            return

        self.create_person_items()
        self.draw_edges()

    def compute_tree_layout(self) -> dict[str, tuple[float, float]]:
        """Compute a simple generation-based layout for the family graph."""
        if self.family is None:
            return {}

        people = list(self.family.fam)
        if not people:
            return {}

        ids = {get_person_id(p) for p in people}
        levels: dict[str, int] = {}

        roots = [p for p in people if p.father is None and p.mother is None]
        if not roots:
            roots = people[:]

        for person in roots:
            levels[get_person_id(person)] = 0

        # Relax constraints a few times. This handles parents that appear after
        # children in the list, and keeps partners on the same row.
        for _ in range(max(4, len(people) * 2)):
            changed = False

            for person in people:
                pid = get_person_id(person)
                parent_levels = []

                if person.father is not None and get_person_id(person.father) in ids:
                    parent_levels.append(levels.get(get_person_id(person.father), 0))
                if person.mother is not None and get_person_id(person.mother) in ids:
                    parent_levels.append(levels.get(get_person_id(person.mother), 0))

                if parent_levels:
                    wanted = max(parent_levels) + 1
                    if levels.get(pid, 0) < wanted:
                        levels[pid] = wanted
                        changed = True
                else:
                    levels.setdefault(pid, 0)

            for person in people:
                pid = get_person_id(person)
                for partner in partners_of(person):
                    qid = get_person_id(partner)
                    if qid not in ids:
                        continue

                    # Partners should share a row, but never by pulling a child up
                    # above their parents. Therefore use the deeper generation.
                    wanted = max(levels.get(pid, 0), levels.get(qid, 0))

                    if levels.get(pid, 0) != wanted:
                        levels[pid] = wanted
                        changed = True
                    if levels.get(qid, 0) != wanted:
                        levels[qid] = wanted
                        changed = True

            if not changed:
                break

        people_by_level: dict[int, list[Person]] = {}
        for person in people:
            people_by_level.setdefault(levels.get(get_person_id(person), 0), []).append(person)

        layout: dict[str, tuple[float, float]] = {}
        x_spacing = 190
        y_spacing = 170

        for level in sorted(people_by_level):
            row_people = people_by_level[level]
            row_ids = {get_person_id(p) for p in row_people}
            placed: set[str] = set()
            ordered_groups: list[list[Person]] = []

            # Keep partner groups adjacent when they are on the same generation.
            for person in row_people:
                pid = get_person_id(person)
                if pid in placed:
                    continue

                group = [person]
                placed.add(pid)

                for partner in partners_of(person):
                    qid = get_person_id(partner)
                    if qid in row_ids and qid not in placed:
                        group.append(partner)
                        placed.add(qid)

                ordered_groups.append(group)

            total_slots = sum(len(group) for group in ordered_groups)
            start_x = -((total_slots - 1) * x_spacing) / 2
            slot = 0

            for group in ordered_groups:
                for person in group:
                    layout[get_person_id(person)] = (start_x + slot * x_spacing, level * y_spacing)
                    slot += 1

        return layout

    def create_person_items(self) -> None:
        if self.family is None:
            return

        layout = self.compute_tree_layout()

        for index, person in enumerate(self.family.fam):
            item = PersonItem(person)
            item.selected_person.connect(self.handle_person_clicked)
            item.right_clicked_person.connect(self.open_person_menu)

            pid = get_person_id(person)
            if pid in self.saved_person_positions:
                item.setPos(self.saved_person_positions[pid])
            else:
                x, y = layout.get(pid, (index * 190, 0))
                item.setPos(x, y)

            self.addItem(item)
            self.person_items[get_person_id(person)] = item

    def add_edge_between(self, source_item, target_item, pen: QPen) -> None:
        edge = EdgeItem(source_item, target_item, pen)
        self.addItem(edge)
        self.edge_items.append(edge)

    def get_person_item(self, person: Optional[Person]) -> Optional[PersonItem]:
        if person is None:
            return None
        return self.person_items.get(get_person_id(person))

    def get_or_create_union_item(self, p1: Person, p2: Person) -> Optional[UnionItem]:
        p1_item = self.get_person_item(p1)
        p2_item = self.get_person_item(p2)

        if p1_item is None or p2_item is None:
            return None

        key = couple_key(p1, p2)
        existing = self.union_items.get(key)
        if existing is not None:
            return existing

        union_item = UnionItem(p1, p2)

        if key in self.saved_union_positions:
            union_item.setPos(self.saved_union_positions[key])
        else:
            p1_center = p1_item.sceneBoundingRect().center()
            p2_center = p2_item.sceneBoundingRect().center()
            union_x = (p1_center.x() + p2_center.x()) / 2
            union_y = max(p1_center.y(), p2_center.y()) + 75
            union_item.setPos(union_x, union_y)

        self.addItem(union_item)
        self.union_items[key] = union_item
        return union_item

    def draw_partner_union(self, p1: Person, p2: Person, partner_pen: QPen, drawn_partner_edges: set) -> Optional[UnionItem]:
        key = couple_key(p1, p2)
        union_item = self.get_or_create_union_item(p1, p2)
        p1_item = self.get_person_item(p1)
        p2_item = self.get_person_item(p2)

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

        partner_pen = QPen(QColor("#0044ff"), 2)
        parent_pen = QPen(QColor("#0033aa"), 1.5)

        drawn_partner_edges = set()
        drawn_children = set()

        # First create union bullets for all explicit partner pairs.
        for person in self.family.fam:
            for partner in partners_of(person):
                if get_person_id(partner) not in self.person_items:
                    continue
                self.draw_partner_union(person, partner, partner_pen, drawn_partner_edges)

        # Then connect children. If a child has two known parents, connect the
        # child to the parents' union bullet. Otherwise connect directly to the
        # known parent.
        for child in self.family.fam:
            child_item = self.get_person_item(child)
            if child_item is None:
                continue

            father = child.father if child.father is not None and get_person_id(child.father) in self.person_items else None
            mother = child.mother if child.mother is not None and get_person_id(child.mother) in self.person_items else None

            if father is not None and mother is not None:
                union_item = self.draw_partner_union(father, mother, partner_pen, drawn_partner_edges)
                if union_item is not None:
                    child_key = (couple_key(father, mother), get_person_id(child))
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

        # Sibling relations are shown in the information panel, but not as lines.

    @Slot(object)
    def handle_person_clicked(self, person: Person) -> None:
        if self.mode is not None:
            if self.source_person is None:
                self.source_person = person
                self.person_selected.emit(person)
                self.status_message.emit(
                    f"{person.name} selected for {connection_mode_label(self.mode)}. "
                    "Now click a different second person."
                )
                return

            if self.source_person is person:
                self.person_selected.emit(person)
                self.status_message.emit(
                    "The second node must be different from the first one. "
                    "Click another person, or press Esc to cancel."
                )
                return

            self.remember_current_positions()
            self.apply_connection(self.source_person, person)

            self.mode = None
            self.source_person = None

            self.rebuild()
            self.person_selected.emit(person)
            return

        self.person_selected.emit(person)

    @Slot(object, object)
    def open_person_menu(self, person: Person, screen_pos) -> None:
        menu = QMenu()

        show_action = menu.addAction("Show information")
        partner_action = menu.addAction("Connect as partner")
        father_action = menu.addAction("This person is father of next clicked")
        mother_action = menu.addAction("This person is mother of next clicked")
        menu.addSeparator()
        remove_partner_action = menu.addAction("Remove partner connection")
        remove_father_action = menu.addAction("Remove father-child connection")
        remove_mother_action = menu.addAction("Remove mother-child connection")
        menu.addSeparator()
        delete_action = menu.addAction("Delete person")

        selected = menu.exec(screen_pos)

        if selected == show_action:
            self.person_selected.emit(person)

        elif selected == partner_action:
            self.start_connection_mode("partner", person)

        elif selected == father_action:
            self.start_connection_mode("father", person)

        elif selected == mother_action:
            self.start_connection_mode("mother", person)

        elif selected == remove_partner_action:
            self.start_connection_mode("remove_partner", person)

        elif selected == remove_father_action:
            self.start_connection_mode("remove_father", person)

        elif selected == remove_mother_action:
            self.start_connection_mode("remove_mother", person)

        elif selected == delete_action:
            if self.family is not None:
                self.remember_current_positions()
                delete_member(self.family, person)
                save_family(self.family)
                self.rebuild()
                self.person_selected.emit(None)

    def start_connection_mode(self, mode: str, source: Optional[Person] = None) -> None:
        self.mode = mode
        self.source_person = source

        if source is None:
            self.status_message.emit(
                f"Connection mode: {connection_mode_label(mode)}. "
                "Click the first person, then click the second person. Press Esc to cancel."
            )
            self.person_selected.emit(None)
            return

        self.status_message.emit(
            f"Connection mode: {connection_mode_label(mode)}. "
            f"Source: {source.name}. Now click another person. Press Esc to cancel."
        )

        self.person_selected.emit(source)

    def cancel_connection_mode(self) -> None:
        if self.mode is None:
            return

        self.mode = None
        self.source_person = None
        self.status_message.emit("Connection mode cancelled.")

    def apply_connection(self, source: Person, target: Person) -> None:
        if self.family is None:
            return

        if source is target:
            self.status_message.emit("Cannot connect a person to themselves.")
            return

        if self.mode == "partner":
            already = are_partners(source, target)
            set_partner(self.family, source, target)
            if already:
                self.status_message.emit(f"{source.name} and {target.name} were already partners.")
            else:
                self.status_message.emit(f"{source.name} and {target.name} are now partners.")

        elif self.mode == "father":
            set_father(self.family, target, source)
            self.status_message.emit(f"{source.name} is now father of {target.name}.")

        elif self.mode == "mother":
            set_mother(self.family, target, source)
            self.status_message.emit(f"{source.name} is now mother of {target.name}.")

        elif self.mode == "remove_partner":
            if remove_partner_connection(self.family, source, target):
                self.status_message.emit(f"Removed partner connection between {source.name} and {target.name}.")
            else:
                self.status_message.emit(f"No partner connection exists between {source.name} and {target.name}.")

        elif self.mode == "remove_father":
            if remove_father_child_connection(self.family, source, target):
                self.status_message.emit(f"Removed father-child connection between {source.name} and {target.name}.")
            else:
                self.status_message.emit(f"No father-child connection exists between {source.name} and {target.name}.")

        elif self.mode == "remove_mother":
            if remove_mother_child_connection(self.family, source, target):
                self.status_message.emit(f"Removed mother-child connection between {source.name} and {target.name}.")
            else:
                self.status_message.emit(f"No mother-child connection exists between {source.name} and {target.name}.")

        elif self.mode == "sibling":
            set_siblings(self.family, source, target)
            self.status_message.emit(f"{source.name} and {target.name} are now siblings.")


# ---------------------------------------------------------------------
# View
# ---------------------------------------------------------------------

class FamilyTreeView(QGraphicsView):
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
        if rect.isNull():
            return

        self.fitInView(rect.adjusted(-80, -80, 80, 80), Qt.KeepAspectRatio)

    def pan(self, dx: int, dy: int) -> None:
        self.horizontalScrollBar().setValue(self.horizontalScrollBar().value() + dx)
        self.verticalScrollBar().setValue(self.verticalScrollBar().value() + dy)

    def wheelEvent(self, event) -> None:
        zoom_in = 1.15
        zoom_out = 1 / zoom_in

        if event.angleDelta().y() > 0:
            self.zoom(zoom_in)
        else:
            self.zoom(zoom_out)

    def keyPressEvent(self, event) -> None:
        key = event.key()

        if key == Qt.Key_C:
            self.center_family_graph()
            event.accept()
            return

        if key in (Qt.Key_Plus, Qt.Key_Equal):
            self.zoom(1.15)
            event.accept()
            return

        if key == Qt.Key_Minus:
            self.zoom(1 / 1.15)
            event.accept()
            return

        pan_step = 45

        if key == Qt.Key_Left:
            self.pan(-pan_step, 0)
            event.accept()
            return

        if key == Qt.Key_Right:
            self.pan(pan_step, 0)
            event.accept()
            return

        if key == Qt.Key_Up:
            self.pan(0, -pan_step)
            event.accept()
            return

        if key == Qt.Key_Down:
            self.pan(0, pan_step)
            event.accept()
            return

        if key == Qt.Key_Escape:
            scene = self.scene()
            if isinstance(scene, FamilyTreeScene):
                scene.cancel_connection_mode()
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
            QWidget {
                background-color: #000000;
            }

            QLabel {
                color: #0044ff;
            }

            QComboBox {
                color: #0044ff;
                background-color: #000000;
                border: 1px solid #0044ff;
                padding: 6px;
            }

            QComboBox QAbstractItemView {
                color: #0044ff;
                background-color: #050510;
                border: 1px solid #0044ff;
                selection-background-color: #101040;
            }

            QPushButton {
                color: #0044ff;
                background-color: #000000;
                border: 1px solid #0044ff;
                padding: 6px;
            }

            QPushButton:hover {
                background-color: #101040;
            }
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

        self.name_input = QLineEdit()
        self.birth_input = QLineEdit()
        self.death_input = QLineEdit()
        self.blood_type_input = QLineEdit()
        self.diseases_input = QLineEdit()
        self.clinical_history_input = QLineEdit()

        self.diseases_input.setPlaceholderText("comma-separated")
        self.clinical_history_input.setPlaceholderText("simple text for now")

        if person is not None:
            self.name_input.setText(person.name)
            self.birth_input.setText(person.birth)
            self.death_input.setText(person.death)
            self.blood_type_input.setText(person.health_info.get("blood_type", ""))

            diseases = person.health_info.get("diseases", [])
            self.diseases_input.setText(", ".join(diseases))

            clinical_history = person.health_info.get("clinical_history", [])
            self.clinical_history_input.setText(str(clinical_history) if clinical_history else "")

        form = QFormLayout()
        form.addRow("Name:", self.name_input)
        form.addRow("Birth:", self.birth_input)
        form.addRow("Death:", self.death_input)
        form.addRow("Blood type:", self.blood_type_input)
        form.addRow("Diseases:", self.diseases_input)
        form.addRow("Clinical history:", self.clinical_history_input)

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

    def values(self) -> dict:
        diseases = [
            item.strip()
            for item in self.diseases_input.text().split(",")
            if item.strip()
        ]

        clinical_text = self.clinical_history_input.text().strip()
        clinical_history = [clinical_text] if clinical_text else []

        return {
            "name": self.name_input.text().strip(),
            "birth": self.birth_input.text().strip(),
            "death": self.death_input.text().strip(),
            "blood_type": self.blood_type_input.text().strip(),
            "diseases": diseases,
            "clinical_history": clinical_history,
        }


# ---------------------------------------------------------------------
# Right-side details panel
# ---------------------------------------------------------------------

class DetailsPanel(QFrame):
    add_person_requested = Signal()
    edit_person_requested = Signal(object)
    delete_person_requested = Signal(object)
    connection_requested = Signal(str)

    def __init__(self):
        super().__init__()

        self.current_person: Optional[Person] = None

        self.setFrameShape(QFrame.StyledPanel)
        self.setMinimumWidth(310)
        self.setMaximumWidth(390)

        self.title = QLabel("No person selected")
        self.title.setStyleSheet("font-size: 18px; font-weight: bold; color: #0044ff;")

        self.info = QLabel("Select or right-click a person node.")
        self.info.setWordWrap(True)
        self.info.setStyleSheet("color: #0044ff;")

        self.add_button = QPushButton("Add person")
        self.edit_button = QPushButton("Edit selected")
        self.delete_button = QPushButton("Delete selected")

        self.partner_button = QPushButton("Connect partners")
        self.father_button = QPushButton("Connect father to child")
        self.mother_button = QPushButton("Connect mother to child")
        self.remove_partner_button = QPushButton("Remove partner connection")
        self.remove_father_button = QPushButton("Remove father-child connection")
        self.remove_mother_button = QPushButton("Remove mother-child connection")

        self.add_button.clicked.connect(self.add_person_requested.emit)
        self.edit_button.clicked.connect(self.emit_edit)
        self.delete_button.clicked.connect(self.emit_delete)

        self.partner_button.clicked.connect(lambda: self.emit_connection("partner"))
        self.father_button.clicked.connect(lambda: self.emit_connection("father"))
        self.mother_button.clicked.connect(lambda: self.emit_connection("mother"))
        self.remove_partner_button.clicked.connect(lambda: self.emit_connection("remove_partner"))
        self.remove_father_button.clicked.connect(lambda: self.emit_connection("remove_father"))
        self.remove_mother_button.clicked.connect(lambda: self.emit_connection("remove_mother"))

        layout = QVBoxLayout(self)

        layout.addWidget(self.title)
        layout.addWidget(self.info)
        layout.addSpacing(18)

        layout.addWidget(self.add_button)
        layout.addWidget(self.edit_button)
        layout.addWidget(self.delete_button)

        layout.addSpacing(18)

        relation_label = QLabel("Two-click connection modes")
        relation_label.setStyleSheet("font-weight: bold; color: #0044ff;")

        layout.addWidget(relation_label)
        layout.addWidget(self.partner_button)
        layout.addWidget(self.father_button)
        layout.addWidget(self.mother_button)
        layout.addSpacing(12)

        remove_label = QLabel("Two-click removal modes")
        remove_label.setStyleSheet("font-weight: bold; color: #0044ff;")
        layout.addWidget(remove_label)
        layout.addWidget(self.remove_partner_button)
        layout.addWidget(self.remove_father_button)
        layout.addWidget(self.remove_mother_button)
        layout.addStretch()

        self.setStyleSheet("""
            QFrame {
                background-color: #050510;
                border-left: 1px solid #0044ff;
            }

            QLabel {
                color: #0044ff;
            }

            QPushButton {
                color: #0044ff;
                background-color: #000000;
                border: 1px solid #0044ff;
                padding: 6px;
            }

            QPushButton:hover {
                background-color: #101040;
            }
        """)

    def set_person(self, person: Optional[Person]) -> None:
        self.current_person = person

        if person is None:
            self.title.setText("No person selected")
            self.info.setText("Select or right-click a person node.")
            return

        blood_type = person.health_info.get("blood_type", "")
        diseases = person.health_info.get("diseases", [])
        clinical_history = person.health_info.get("clinical_history", [])

        self.title.setText(person.name)

        self.info.setText(
            f"ID: {person.identifier}\n\n"
            f"Personal information\n"
            f"Birth: {person.birth or 'Unknown'}\n"
            f"Death: {person.death or 'None'}\n"
            f"Blood type: {blood_type or 'Unknown'}\n"
            f"Diseases: {', '.join(diseases) if diseases else 'None'}\n"
            f"Clinical history: {clinical_history if clinical_history else 'None'}\n\n"
            f"Family relations\n"
            f"Father: {person_label(person.father)}\n"
            f"Mother: {person_label(person.mother)}\n"
            f"Partners: {list_names(partners_of(person))}\n"
            f"Kids: {list_names(person.kids)}\n"
            f"Siblings: {list_names(person.siblings)}"
        )

    def emit_edit(self) -> None:
        if self.current_person is not None:
            self.edit_person_requested.emit(self.current_person)

    def emit_delete(self) -> None:
        if self.current_person is not None:
            self.delete_person_requested.emit(self.current_person)

    def emit_connection(self, mode: str) -> None:
        self.connection_requested.emit(mode)


# ---------------------------------------------------------------------
# Main window
# ---------------------------------------------------------------------

class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()

        self.setWindowTitle("FamilyTree")
        self.resize(1150, 740)

        self.family: Optional[Family] = None

        self.scene = FamilyTreeScene()
        self.view = FamilyTreeView(self.scene)
        self.details = DetailsPanel()

        self.scene.person_selected.connect(self.details.set_person)
        self.scene.status_message.connect(self.statusBar().showMessage)

        self.details.add_person_requested.connect(self.add_person)
        self.details.edit_person_requested.connect(self.edit_person)
        self.details.delete_person_requested.connect(self.delete_person)
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

        self.setStyleSheet("""
            QMainWindow {
                background-color: #000000;
            }

            QStatusBar {
                color: #0044ff;
                background-color: #050510;
                border-top: 1px solid #0044ff;
            }

            QToolBar {
                background-color: #050510;
                border-bottom: 1px solid #0044ff;
                spacing: 8px;
            }

            QToolButton {
                color: #0044ff;
                background-color: #000000;
                border: 1px solid #0044ff;
                padding: 6px;
            }

            QToolButton:hover {
                background-color: #101040;
            }

            QMenu {
                background-color: #050510;
                color: #0044ff;
                border: 1px solid #0044ff;
            }

            QMenu::item:selected {
                background-color: #101040;
            }

            QDialog {
                background-color: #050510;
                color: #0044ff;
            }

            QLabel {
                color: #0044ff;
            }

            QLineEdit {
                color: #0044ff;
                background-color: #000000;
                border: 1px solid #0044ff;
                padding: 4px;
            }

            QPushButton {
                color: #0044ff;
                background-color: #000000;
                border: 1px solid #0044ff;
                padding: 6px;
            }

            QPushButton:hover {
                background-color: #101040;
            }

            QInputDialog {
                background-color: #050510;
            }
        """)

    def create_toolbar(self) -> None:
        toolbar = QToolBar("Main toolbar")
        self.addToolBar(toolbar)

        add_family_action = QAction("Add family", self)
        load_family_action = QAction("Load family", self)
        save_family_action = QAction("Save family", self)
        show_family_action = QAction("Show family", self)
        initial_screen_action = QAction("Initial screen", self)
        add_person_action = QAction("Add person", self)
        center_action = QAction("Center graph", self)
        connect_partners_action = QAction("Connect partners", self)
        connect_father_action = QAction("Connect father to child", self)
        connect_mother_action = QAction("Connect mother to child", self)
        remove_partner_action = QAction("Remove partners", self)
        remove_father_action = QAction("Remove father-child", self)
        remove_mother_action = QAction("Remove mother-child", self)

        add_family_action.triggered.connect(self.create_family)
        load_family_action.triggered.connect(self.load_existing_family)
        save_family_action.triggered.connect(self.save_current_family)
        show_family_action.triggered.connect(self.show_family)
        initial_screen_action.triggered.connect(self.initial_screen)
        add_person_action.triggered.connect(self.add_person)
        center_action.triggered.connect(self.view.center_family_graph)
        connect_partners_action.triggered.connect(lambda: self.start_two_click_connection("partner"))
        connect_father_action.triggered.connect(lambda: self.start_two_click_connection("father"))
        connect_mother_action.triggered.connect(lambda: self.start_two_click_connection("mother"))
        remove_partner_action.triggered.connect(lambda: self.start_two_click_connection("remove_partner"))
        remove_father_action.triggered.connect(lambda: self.start_two_click_connection("remove_father"))
        remove_mother_action.triggered.connect(lambda: self.start_two_click_connection("remove_mother"))

        toolbar.addAction(add_family_action)
        toolbar.addAction(load_family_action)
        toolbar.addAction(save_family_action)
        toolbar.addAction(show_family_action)
        toolbar.addAction(initial_screen_action)
        toolbar.addSeparator()
        toolbar.addAction(add_person_action)
        toolbar.addAction(center_action)
        toolbar.addSeparator()
        toolbar.addAction(connect_partners_action)
        toolbar.addAction(connect_father_action)
        toolbar.addAction(connect_mother_action)
        toolbar.addSeparator()
        toolbar.addAction(remove_partner_action)
        toolbar.addAction(remove_father_action)
        toolbar.addAction(remove_mother_action)

    def show_initial_page(self) -> None:
        self.initial_page.refresh_families()
        self.stack.setCurrentWidget(self.initial_page)

    def show_graph_page(self) -> None:
        self.stack.setCurrentWidget(self.graph_page)
        self.view.setFocus()

    def set_active_family(self, family: Family) -> None:
        self.family = family
        self.scene.saved_person_positions.clear()
        self.scene.saved_union_positions.clear()
        self.scene.set_family(self.family, preserve_positions=False)
        self.details.set_person(None)
        self.show_graph_page()
        self.view.center_family_graph()

    @Slot()
    def create_family(self) -> None:
        name, ok = QInputDialog.getText(self, "Create family", "Family name:")

        if not ok or not name.strip():
            return

        family = init_family(name.strip())
        save_family(family)

        self.set_active_family(family)
        self.initial_page.refresh_families()

        self.statusBar().showMessage(f"Created family: {self.family.fam_name}")

    @Slot(str)
    def load_family_by_name(self, selected: str) -> None:
        if not selected:
            return

        family_id = family_id_from_stem(selected)
        family = load_family(selected, family_id)
        self.set_active_family(family)

        self.statusBar().showMessage(f"Loaded family: {self.family.fam_name}")

    @Slot()
    def load_existing_family(self) -> None:
        choices = available_family_names()

        if not choices:
            QMessageBox.information(
                self,
                "No families found",
                f"No family files found in:\n{DATA_SOURCE}",
            )
            return

        selected, ok = QInputDialog.getItem(
            self,
            "Load family",
            "Choose a family:",
            choices,
            0,
            False,
        )

        if not ok or not selected:
            return

        self.load_family_by_name(selected)

    @Slot()
    def save_current_family(self) -> None:
        if self.family is None:
            QMessageBox.information(self, "No family", "Create or load a family first.")
            return

        save_family(self.family)
        self.statusBar().showMessage(f"Saved family: {self.family.fam_name}")

    @Slot()
    def show_family(self) -> None:
        if self.family is None:
            self.show_initial_page()
            self.statusBar().showMessage("Select or create a family first.")
            return

        self.scene.set_family(self.family)
        self.show_graph_page()
        self.view.center_family_graph()
        self.statusBar().showMessage(f"Showing family: {self.family.fam_name}")

    @Slot()
    def initial_screen(self) -> None:
        self.scene.clear_to_initial_screen()
        self.details.set_person(None)
        self.show_initial_page()
        self.statusBar().showMessage("Initial screen")

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

        person = Person(
            name=values["name"],
            birth=values["birth"],
            death=values["death"],
            blood_type=values["blood_type"],
            diseases=values["diseases"],
            clinical_history=values["clinical_history"],
            father=None,
            mother=None,
            siblings=None,
            partners=None,
            kids=None,
        )

        add_new_member(self.family, person)
        save_family(self.family)

        self.scene.set_family(self.family, preserve_positions=True)
        self.details.set_person(person)
        self.show_graph_page()

        self.statusBar().showMessage(f"Added person: {person.name}")

    @Slot(object)
    def edit_person(self, person: Person) -> None:
        if self.family is None:
            return

        dialog = PersonDialog(self, person)

        if dialog.exec() != QDialog.Accepted:
            return

        values = dialog.values()

        if not values["name"]:
            QMessageBox.warning(self, "Invalid person", "Name cannot be empty.")
            return

        upd_member_info(
            self.family,
            person.identifier,
            values["name"],
            values["birth"],
            values["death"],
            values["blood_type"],
            values["diseases"],
            values["clinical_history"],
            person.father,
            person.mother,
            partners_of(person),
        )

        save_family(self.family)

        item = self.scene.get_person_item(person)
        if item is not None:
            item.update()
        self.details.set_person(person)

        self.statusBar().showMessage(f"Updated person: {person.name}")

    @Slot(object)
    def delete_person(self, person: Person) -> None:
        if self.family is None:
            return

        answer = QMessageBox.question(
            self,
            "Delete person",
            f"Delete {person.name} from {self.family.fam_name}?",
        )

        if answer != QMessageBox.Yes:
            return

        delete_member(self.family, person)
        save_family(self.family)

        self.scene.set_family(self.family, preserve_positions=True)
        self.details.set_person(None)

        self.statusBar().showMessage(f"Deleted person: {person.name}")

    @Slot(str)
    def start_two_click_connection(self, mode: str) -> None:
        if self.family is None:
            QMessageBox.information(self, "No family", "Create or load a family first.")
            self.show_initial_page()
            return

        self.show_graph_page()
        self.scene.start_connection_mode(mode)
        self.view.setFocus()

    @Slot(str, object)
    def start_connection_mode(self, mode: str, person: Person) -> None:
        self.scene.start_connection_mode(mode, person)
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
