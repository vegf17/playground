"""Generate rich FamilyTree test data.

Run from the same folder as backend.py/classes.py:
    python generate_test_families.py --reset

The script creates three connected families with shared people across families,
multiple generations, multiple partners, diseases, and empty photo fields.
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
from pathlib import Path
from typing import Any

# Make backend.py use ./data relative to this file's folder, no matter where
# the script is launched from.
BASE_DIR = Path(__file__).resolve().parent
os.chdir(BASE_DIR)

from backend import (  # noqa: E402
    DATA_SOURCE,
    FAM_DIR,
    PPL_DIR,
    PPL_FILE,
    add_person,
    init_count_file,
    init_family,
    start,
)


def relation(
    *,
    father: str | None = None,
    mother: str | None = None,
    siblings: list[str] | None = None,
    partners: list[str] | None = None,
    kids: list[str] | None = None,
) -> dict[str, Any]:
    return {
        "father": father,
        "mother": mother,
        "siblings": siblings or [],
        "partners": partners or [],
        "kids": kids or [],
    }


def reset_data_folder() -> None:
    data_path = BASE_DIR / DATA_SOURCE
    if data_path.exists():
        shutil.rmtree(data_path)


def make_person(
    people: dict[str, Any],
    key: str,
    name: str,
    birth: str,
    *,
    death: str = "",
    blood_type: str = "Unknown",
    diseases: list[str] | None = None,
) -> str:
    person = add_person(
        name,
        birth=birth,
        death=death,
        blood_type=blood_type,
        diseases=diseases or [],
        photo="",
    )
    people[key] = person
    return person.identifier


def create_people() -> dict[str, Any]:
    people: dict[str, Any] = {}

    # Silva line: six visible generations.
    make_person(people, "manuel_silva", "Manuel Silva", "1910", death="1988", blood_type="O+", diseases=["Hypertension"])
    make_person(people, "teresa_silva", "Teresa Almeida Silva", "1913", death="1994", blood_type="A+", diseases=["Type 2 diabetes"])
    make_person(people, "antonio_silva", "António Silva", "1935", death="2012", blood_type="O+", diseases=["Asthma"])
    make_person(people, "maria_costa", "Maria Costa Silva", "1938", death="2018", blood_type="B+", diseases=[])
    make_person(people, "carlos_silva", "Carlos Silva", "1961", blood_type="A+", diseases=["High cholesterol"])
    make_person(people, "ana_silva", "Ana Silva", "1964", blood_type="O-", diseases=[])
    make_person(people, "helena_rocha", "Helena Rocha", "1963", blood_type="AB+", diseases=["Migraine"])
    make_person(people, "beatriz_mendes", "Beatriz Mendes", "1968", blood_type="A-", diseases=[])
    make_person(people, "diana_silva", "Diana Silva", "1987", blood_type="A+", diseases=[])
    make_person(people, "ricardo_silva", "Ricardo Silva", "1990", blood_type="O+", diseases=["Peanut allergy"])
    make_person(people, "lucas_silva", "Lucas Silva", "1998", blood_type="A-", diseases=[])
    make_person(people, "miguel_matos", "Miguel Matos", "1985", blood_type="B+", diseases=["Celiac disease"])
    make_person(people, "sofia_pereira", "Sofia Pereira", "1989", blood_type="O+", diseases=[])
    make_person(people, "gabriel_silva", "Gabriel Silva", "2012", blood_type="A+", diseases=["Seasonal allergies"])
    make_person(people, "ines_silva", "Inês Silva", "2035", blood_type="A+", diseases=[])

    # Rocha line, connected through Helena Rocha.
    make_person(people, "joaquim_rocha", "Joaquim Rocha", "1908", death="1981", blood_type="B+", diseases=["Heart disease"])
    make_person(people, "laura_rocha", "Laura Ferreira Rocha", "1912", death="1990", blood_type="A+", diseases=[])
    make_person(people, "fernando_rocha", "Fernando Rocha", "1936", death="2010", blood_type="AB+", diseases=["Hypertension"])
    make_person(people, "celeste_nunes", "Celeste Nunes Rocha", "1939", blood_type="O+", diseases=[])
    make_person(people, "paulo_rocha", "Paulo Rocha", "1966", blood_type="B-", diseases=["Type 1 diabetes"])
    make_person(people, "lara_rocha", "Lara Rocha", "1992", blood_type="O+", diseases=[])
    make_person(people, "tiago_rocha", "Tiago Rocha", "2018", blood_type="A+", diseases=[])

    # Matos line, connected through Miguel Matos and descendants.
    make_person(people, "alberto_matos", "Alberto Matos", "1915", death="1999", blood_type="B+", diseases=[])
    make_person(people, "rosa_matos", "Rosa Martins Matos", "1918", death="2004", blood_type="A+", diseases=["Osteoporosis"])
    make_person(people, "eduardo_matos", "Eduardo Matos", "1942", blood_type="B+", diseases=["Arthritis"])
    make_person(people, "clara_lopes", "Clara Lopes Matos", "1945", blood_type="O+", diseases=[])
    make_person(people, "joana_matos", "Joana Matos", "1972", blood_type="B-", diseases=["Thyroid disease"])
    make_person(people, "pedro_matos", "Pedro Matos", "1980", blood_type="O-", diseases=[])
    make_person(people, "mariana_matos", "Mariana Matos", "2015", blood_type="B+", diseases=[])

    return people


def pid(people: dict[str, Any], key: str) -> str:
    return people[key].identifier


def create_silva_family(people: dict[str, Any]) -> None:
    members = [
        pid(people, "manuel_silva"), pid(people, "teresa_silva"),
        pid(people, "antonio_silva"), pid(people, "maria_costa"),
        pid(people, "carlos_silva"), pid(people, "ana_silva"),
        pid(people, "helena_rocha"), pid(people, "beatriz_mendes"),
        pid(people, "diana_silva"), pid(people, "ricardo_silva"), pid(people, "lucas_silva"),
        pid(people, "miguel_matos"), pid(people, "sofia_pereira"),
        pid(people, "gabriel_silva"), pid(people, "ines_silva"),
    ]
    r = {
        pid(people, "manuel_silva"): relation(partners=[pid(people, "teresa_silva")], kids=[pid(people, "antonio_silva")]),
        pid(people, "teresa_silva"): relation(partners=[pid(people, "manuel_silva")], kids=[pid(people, "antonio_silva")]),
        pid(people, "antonio_silva"): relation(father=pid(people, "manuel_silva"), mother=pid(people, "teresa_silva"), partners=[pid(people, "maria_costa")], kids=[pid(people, "carlos_silva"), pid(people, "ana_silva")]),
        pid(people, "maria_costa"): relation(partners=[pid(people, "antonio_silva")], kids=[pid(people, "carlos_silva"), pid(people, "ana_silva")]),
        pid(people, "carlos_silva"): relation(father=pid(people, "antonio_silva"), mother=pid(people, "maria_costa"), siblings=[pid(people, "ana_silva")], partners=[pid(people, "helena_rocha"), pid(people, "beatriz_mendes")], kids=[pid(people, "diana_silva"), pid(people, "ricardo_silva"), pid(people, "lucas_silva")]),
        pid(people, "ana_silva"): relation(father=pid(people, "antonio_silva"), mother=pid(people, "maria_costa"), siblings=[pid(people, "carlos_silva")]),
        pid(people, "helena_rocha"): relation(partners=[pid(people, "carlos_silva")], kids=[pid(people, "diana_silva"), pid(people, "ricardo_silva")]),
        pid(people, "beatriz_mendes"): relation(partners=[pid(people, "carlos_silva")], kids=[pid(people, "lucas_silva")]),
        pid(people, "diana_silva"): relation(father=pid(people, "carlos_silva"), mother=pid(people, "helena_rocha"), siblings=[pid(people, "ricardo_silva"), pid(people, "lucas_silva")], partners=[pid(people, "miguel_matos")], kids=[pid(people, "gabriel_silva")]),
        pid(people, "ricardo_silva"): relation(father=pid(people, "carlos_silva"), mother=pid(people, "helena_rocha"), siblings=[pid(people, "diana_silva"), pid(people, "lucas_silva")]),
        pid(people, "lucas_silva"): relation(father=pid(people, "carlos_silva"), mother=pid(people, "beatriz_mendes"), siblings=[pid(people, "diana_silva"), pid(people, "ricardo_silva")]),
        pid(people, "miguel_matos"): relation(partners=[pid(people, "diana_silva")], kids=[pid(people, "gabriel_silva")]),
        pid(people, "sofia_pereira"): relation(partners=[pid(people, "gabriel_silva")], kids=[pid(people, "ines_silva")]),
        pid(people, "gabriel_silva"): relation(father=pid(people, "miguel_matos"), mother=pid(people, "diana_silva"), partners=[pid(people, "sofia_pereira")], kids=[pid(people, "ines_silva")]),
        pid(people, "ines_silva"): relation(father=pid(people, "gabriel_silva"), mother=pid(people, "sofia_pereira")),
    }
    init_family("Silva Deep Test Family", members=members, relations=r)


def create_rocha_family(people: dict[str, Any]) -> None:
    members = [
        pid(people, "joaquim_rocha"), pid(people, "laura_rocha"),
        pid(people, "fernando_rocha"), pid(people, "celeste_nunes"),
        pid(people, "helena_rocha"), pid(people, "paulo_rocha"),
        pid(people, "carlos_silva"), pid(people, "diana_silva"), pid(people, "ricardo_silva"),
        pid(people, "lara_rocha"), pid(people, "tiago_rocha"),
    ]
    r = {
        pid(people, "joaquim_rocha"): relation(partners=[pid(people, "laura_rocha")], kids=[pid(people, "fernando_rocha")]),
        pid(people, "laura_rocha"): relation(partners=[pid(people, "joaquim_rocha")], kids=[pid(people, "fernando_rocha")]),
        pid(people, "fernando_rocha"): relation(father=pid(people, "joaquim_rocha"), mother=pid(people, "laura_rocha"), partners=[pid(people, "celeste_nunes")], kids=[pid(people, "helena_rocha"), pid(people, "paulo_rocha")]),
        pid(people, "celeste_nunes"): relation(partners=[pid(people, "fernando_rocha")], kids=[pid(people, "helena_rocha"), pid(people, "paulo_rocha")]),
        pid(people, "helena_rocha"): relation(father=pid(people, "fernando_rocha"), mother=pid(people, "celeste_nunes"), siblings=[pid(people, "paulo_rocha")], partners=[pid(people, "carlos_silva")], kids=[pid(people, "diana_silva"), pid(people, "ricardo_silva")]),
        pid(people, "paulo_rocha"): relation(father=pid(people, "fernando_rocha"), mother=pid(people, "celeste_nunes"), siblings=[pid(people, "helena_rocha")], kids=[pid(people, "lara_rocha")]),
        pid(people, "carlos_silva"): relation(partners=[pid(people, "helena_rocha")], kids=[pid(people, "diana_silva"), pid(people, "ricardo_silva")]),
        pid(people, "diana_silva"): relation(father=pid(people, "carlos_silva"), mother=pid(people, "helena_rocha"), siblings=[pid(people, "ricardo_silva")]),
        pid(people, "ricardo_silva"): relation(father=pid(people, "carlos_silva"), mother=pid(people, "helena_rocha"), siblings=[pid(people, "diana_silva")]),
        pid(people, "lara_rocha"): relation(father=pid(people, "paulo_rocha"), kids=[pid(people, "tiago_rocha")]),
        pid(people, "tiago_rocha"): relation(mother=pid(people, "lara_rocha")),
    }
    init_family("Rocha Shared Branch", members=members, relations=r)


def create_matos_family(people: dict[str, Any]) -> None:
    members = [
        pid(people, "alberto_matos"), pid(people, "rosa_matos"),
        pid(people, "eduardo_matos"), pid(people, "clara_lopes"),
        pid(people, "miguel_matos"), pid(people, "joana_matos"), pid(people, "pedro_matos"),
        pid(people, "diana_silva"), pid(people, "gabriel_silva"), pid(people, "sofia_pereira"),
        pid(people, "ines_silva"), pid(people, "mariana_matos"),
    ]
    r = {
        pid(people, "alberto_matos"): relation(partners=[pid(people, "rosa_matos")], kids=[pid(people, "eduardo_matos")]),
        pid(people, "rosa_matos"): relation(partners=[pid(people, "alberto_matos")], kids=[pid(people, "eduardo_matos")]),
        pid(people, "eduardo_matos"): relation(father=pid(people, "alberto_matos"), mother=pid(people, "rosa_matos"), partners=[pid(people, "clara_lopes")], kids=[pid(people, "miguel_matos"), pid(people, "joana_matos"), pid(people, "pedro_matos")]),
        pid(people, "clara_lopes"): relation(partners=[pid(people, "eduardo_matos")], kids=[pid(people, "miguel_matos"), pid(people, "joana_matos"), pid(people, "pedro_matos")]),
        pid(people, "miguel_matos"): relation(father=pid(people, "eduardo_matos"), mother=pid(people, "clara_lopes"), siblings=[pid(people, "joana_matos"), pid(people, "pedro_matos")], partners=[pid(people, "diana_silva")], kids=[pid(people, "gabriel_silva")]),
        pid(people, "joana_matos"): relation(father=pid(people, "eduardo_matos"), mother=pid(people, "clara_lopes"), siblings=[pid(people, "miguel_matos"), pid(people, "pedro_matos")], kids=[pid(people, "mariana_matos")]),
        pid(people, "pedro_matos"): relation(father=pid(people, "eduardo_matos"), mother=pid(people, "clara_lopes"), siblings=[pid(people, "miguel_matos"), pid(people, "joana_matos")]),
        pid(people, "diana_silva"): relation(partners=[pid(people, "miguel_matos")], kids=[pid(people, "gabriel_silva")]),
        pid(people, "gabriel_silva"): relation(father=pid(people, "miguel_matos"), mother=pid(people, "diana_silva"), partners=[pid(people, "sofia_pereira")], kids=[pid(people, "ines_silva")]),
        pid(people, "sofia_pereira"): relation(partners=[pid(people, "gabriel_silva")], kids=[pid(people, "ines_silva")]),
        pid(people, "ines_silva"): relation(father=pid(people, "gabriel_silva"), mother=pid(people, "sofia_pereira")),
        pid(people, "mariana_matos"): relation(mother=pid(people, "joana_matos")),
    }
    init_family("Matos Shared Descendants", members=members, relations=r)


def print_summary() -> None:
    people_path = BASE_DIR / DATA_SOURCE / PPL_DIR / PPL_FILE
    fam_dir = BASE_DIR / DATA_SOURCE / FAM_DIR
    with open(people_path, "r", encoding="utf-8") as f:
        people_data = json.load(f)
    family_files = sorted(fam_dir.glob("*.json"))

    shared = {
        person_id: data
        for person_id, data in people_data.items()
        if len(data.get("families", [])) >= 2
    }

    print("\nGenerated test data")
    print("===================")
    print(f"People: {len(people_data)}")
    print(f"Families: {len(family_files)}")
    print("Family files:")
    for path in family_files:
        print(f"  - {path.relative_to(BASE_DIR)}")
    print("Shared people across families:")
    for person_id, data in shared.items():
        print(f"  - {data['name']} ({person_id}): {', '.join(data.get('families', []))}")


def main() -> None:
    parser = argparse.ArgumentParser(description="Generate FamilyTree test data.")
    parser.add_argument("--reset", action="store_true", help="Delete ./data before generating the test dataset.")
    args = parser.parse_args()

    if args.reset:
        reset_data_folder()

    start()
    init_count_file()
    people = create_people()
    create_silva_family(people)
    create_rocha_family(people)
    create_matos_family(people)
    print_summary()


if __name__ == "__main__":
    main()
