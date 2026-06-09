"""Sample FamilyTree data built for the current classes.py model.

It creates:
- 3 Family objects: f0, f1, f2
- one merged people dictionary containing one Person object per unique person
- multiple people that belong to more than one family
- family depth of at least 6 generations
- Person.photo is intentionally the only empty Person field

Usage:
    from sample_family_data import people, families
"""

from __future__ import annotations

from classes import Family, Person, family_to_json, person_to_json


RELATION_KEYS = ["father", "mother", "siblings", "partners", "kids"]


def empty_rel() -> dict:
    return {
        "father": None,
        "mother": None,
        "siblings": [],
        "partners": [],
        "kids": [],
    }


def add_unique(xs: list, x) -> None:
    if x not in xs:
        xs.append(x)


def build_relations(members: list[str], parent_links: dict[str, tuple[str | None, str | None]], partner_links: list[tuple[str, str]]) -> dict[str, dict]:
    """Build a relation dictionary compatible with Family.relations."""
    relations = {person_id: empty_rel() for person_id in members}

    # Partners are symmetric.
    for a, b in partner_links:
        if a in relations and b in relations and a != b:
            add_unique(relations[a]["partners"], b)
            add_unique(relations[b]["partners"], a)

    # Parents and kids.
    for child, parents in parent_links.items():
        if child not in relations:
            continue
        father, mother = parents
        if father in relations:
            relations[child]["father"] = father
            add_unique(relations[father]["kids"], child)
        if mother in relations:
            relations[child]["mother"] = mother
            add_unique(relations[mother]["kids"], child)

        if father in relations and mother in relations:
            add_unique(relations[father]["partners"], mother)
            add_unique(relations[mother]["partners"], father)

    # Siblings: same known father or same known mother.
    for person_id in members:
        father = relations[person_id]["father"]
        mother = relations[person_id]["mother"]
        for other_id in members:
            if person_id == other_id:
                continue
            same_father = father is not None and father == relations[other_id]["father"]
            same_mother = mother is not None and mother == relations[other_id]["mother"]
            if same_father or same_mother:
                add_unique(relations[person_id]["siblings"], other_id)

    return relations


# ---------------------------------------------------------------------
# Person data, merged across all families
# ---------------------------------------------------------------------

PERSON_SPECS = {
    # Shared/Almeida-Carvalho main branch, used by f0 and partly by f2.
    "p0":  ("Artur Carvalho", "1896-03-14", "1978-09-02", "O+", ["hypertension"]),
    "p1":  ("Beatriz Almeida", "1898-11-27", "1985-01-18", "A+", []),
    "p2":  ("Clara Carvalho", "1921-05-03", "2006-07-11", "A+", ["type 2 diabetes"]),
    "p3":  ("David Martins", "1919-02-20", "1999-12-04", "B+", []),
    "p4":  ("Eduardo Carvalho", "1944-08-16", "Alive", "AB+", ["asthma"]),
    "p5":  ("Flora Costa", "1946-10-08", "Alive", "O-", []),
    "p6":  ("Gabriel Carvalho", "1969-01-25", "Alive", "O+", []),
    "p7":  ("Helena Duarte", "1971-06-19", "Alive", "A-", ["migraine"]),
    "p8":  ("Ines Carvalho", "1994-04-12", "Alive", "A+", []),
    "p9":  ("Joao Ferreira", "1992-09-30", "Alive", "B-", ["eczema"]),
    "p10": ("Lara Carvalho", "2020-02-17", "Alive", "O+", []),

    # Bennett/Silva branch, connected to f0 through Ines Carvalho.
    "p20": ("Thomas Bennett", "1892-01-05", "1969-03-13", "B+", []),
    "p21": ("Uma Silva", "1895-12-22", "1976-05-09", "O+", ["arthritis"]),
    "p22": ("Victor Bennett", "1918-07-07", "1992-10-21", "A+", []),
    "p23": ("Wendy Rocha", "1920-03-29", "2001-02-14", "A-", ["hypertension"]),
    "p24": ("Xavier Bennett", "1948-09-01", "Alive", "O-", []),
    "p25": ("Yara Bennett", "1975-11-12", "Alive", "B+", ["celiac disease"]),
    "p26": ("Zane Oliveira", "1974-05-18", "Alive", "AB-", []),
    "p27": ("Alice Oliveira", "1998-08-03", "Alive", "A+", []),
    "p28": ("Bruno Mendes", "1996-02-26", "Alive", "O+", ["allergic rhinitis"]),
    "p29": ("Cora Mendes", "2023-12-09", "Alive", "A+", []),

    # Moreira/Lima branch, connected to f0 through Gabriel Carvalho.
    "p30": ("Otto Moreira", "1889-04-17", "1961-06-24", "O-", ["glaucoma"]),
    "p31": ("Paloma Lima", "1891-08-02", "1970-11-30", "B-", []),
    "p32": ("Quentin Moreira", "1915-01-19", "1988-04-06", "AB+", []),
    "p33": ("Rosa Nunes", "1917-10-28", "1995-08-15", "A+", ["osteoporosis"]),
    "p34": ("Sofia Moreira", "1942-12-13", "Alive", "B+", []),
    "p36": ("Ursula Carvalho-Moreira", "1972-03-22", "Alive", "O+", ["hypothyroidism"]),
    "p37": ("Nuno Pereira", "1970-07-09", "Alive", "A-", []),
    "p38": ("Vera Pereira", "1997-01-31", "Alive", "O+", []),
    "p39": ("Luis Ramos", "1995-06-06", "Alive", "B+", ["asthma"]),
    "p40": ("Marta Ramos", "2024-04-21", "Alive", "AB+", []),
}


# ---------------------------------------------------------------------
# Family definitions
# ---------------------------------------------------------------------

FAMILY_MEMBERS = {
    "f0": ["p0", "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10"],
    "f1": ["p20", "p21", "p22", "p23", "p24", "p8", "p25", "p26", "p27", "p28", "p29"],
    "f2": ["p30", "p31", "p32", "p33", "p34", "p6", "p36", "p37", "p38", "p39", "p40"],
}

FAMILY_NAMES = {
    "f0": "Carvalho-Almeida Family",
    "f1": "Bennett-Silva Family",
    "f2": "Moreira-Lima Family",
}

PARENT_LINKS = {
    # Six-generation line: p0/p1 -> p2 -> p4 -> p6 -> p8 -> p10
    "f0": {
        "p2": ("p0", "p1"),
        "p4": ("p3", "p2"),
        "p6": ("p4", "p5"),
        "p8": ("p6", "p7"),
        "p10": ("p9", "p8"),
    },
    # Six-generation line: p20/p21 -> p22 -> p24 -> p25 -> p27 -> p29
    # Ines Carvalho (p8) is shared with f0 and is a partner in this family.
    "f1": {
        "p22": ("p20", "p21"),
        "p24": ("p22", "p23"),
        "p25": ("p24", "p8"),
        "p27": ("p26", "p25"),
        "p29": ("p28", "p27"),
    },
    # Six-generation line: p30/p31 -> p32 -> p34 -> p36 -> p38 -> p40
    # Gabriel Carvalho (p6) is shared with f0 and is a partner in this family.
    "f2": {
        "p32": ("p30", "p31"),
        "p34": ("p32", "p33"),
        "p36": ("p6", "p34"),
        "p38": ("p37", "p36"),
        "p40": ("p39", "p38"),
    },
}

PARTNER_LINKS = {
    "f0": [("p0", "p1"), ("p3", "p2"), ("p4", "p5"), ("p6", "p7"), ("p9", "p8")],
    "f1": [("p20", "p21"), ("p22", "p23"), ("p24", "p8"), ("p26", "p25"), ("p28", "p27")],
    "f2": [("p30", "p31"), ("p32", "p33"), ("p6", "p34"), ("p37", "p36"), ("p39", "p38")],
}


def families_for_person(person_id: str) -> list[str]:
    return [family_id for family_id, members in FAMILY_MEMBERS.items() if person_id in members]


people = {
    person_id: Person(
        name=name,
        birth=birth,
        death=death,
        health_info={
            "blood_type": blood_type,
            "diseases": diseases,
            "clinical_history": ["No registered clinical notes."],
        },
        photo="",
        identifier=person_id,
        families=families_for_person(person_id),
    )
    for person_id, (name, birth, death, blood_type, diseases) in PERSON_SPECS.items()
}

families = {
    family_id: Family(
        name=FAMILY_NAMES[family_id],
        identifier=family_id,
        members=members,
        relations=build_relations(
            members=members,
            parent_links=PARENT_LINKS[family_id],
            partner_links=PARTNER_LINKS[family_id],
        ),
    )
    for family_id, members in FAMILY_MEMBERS.items()
}


# Optional JSON-ready dictionaries, useful for writing to your data files.
people_json = {}
for person in people.values():
    people_json.update(person_to_json(person))

families_json = {}
for family in families.values():
    families_json.update(family_to_json(family))


if __name__ == "__main__":
    print(f"Created {len(families)} families and {len(people)} unique people.")
    print("Shared people:")
    for person_id, person in people.items():
        if len(person.families) > 1:
            print(f"  {person_id}: {person.name} -> {person.families}")
