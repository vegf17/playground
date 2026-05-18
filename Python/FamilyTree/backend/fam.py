from person import Person, Family
from pathlib import Path
import json
#import streamlit as st
#import graphviz as gvz

DATA_DIR = Path("./data/families")
DATA_DIR.mkdir(parents=True, exist_ok=True)


def init_family(fam_name):
    return Family(fam_name)


def save_family(family):
    """
    family: Family
    """
    filename = family.fam_name.replace(" ", "_").lower() + ".json"
    path = DATA_DIR / filename

    with open(path, "w", encoding="utf-8") as f:
        json.dump(family.create_dict(), f, indent=4, ensure_ascii=False)
    print(family.fam_name + " was created in " + f"{path}")


def load_family(fam_name):
    """
    fam_name: Family name or json family name
    """    
    filename = fam_name.replace(" ", "_").lower() + ".json"
    path = DATA_DIR / filename

    with open(path, "r") as f:
        data = json.load(f)

    return family_from_dict(data)


def add_new_member(family, person):
    """
    family: Family
    person: Person
    """
    person.identifier = "p"+f"{family.count_id}"
    family.count_id = family.count_id+1
    family.fam.append(person)

    #if the person has a partner, which is in fam but does not have a partner, then update that info
    if person.partner is not None and person.partner in family.fam: #the partner exists and it is in fam
        person.partner.partner=person

    save_family(family)
    #family.fam_graph.node(f"{family.fam.index(person)}", person.name)
    
    return family


#Updates the information of a given person
def upd_member_info(family,
                    person_id,
                    name,
                    birth,
                    death,
                    blood_type,
                    diseases,
                    clinical_history,
                    father,
                    mother,
                    partner
                    ):
    p = get_member(family, person_id)
    p.name = name
    p.birth = birth
    p.death = death
    p.health_info["blood_type"] = blood_type
    p.health_info["diseases"] = diseases
    p.health_info["clinical_history"] = clinical_history
    p.father = father
    p.mother = mother
    p.partner = partner

    upd_family_relations(family, p)
    
    return family

#given a family and a person identifier, returns the respective person
def get_member(family, person_id):
    for p in family.fam:
        if p.identifier==person_id:
            return p
    print(f"There is no person with the identifier: {person_id}")


#given a fmaily and a person identifier, checks if the person exists
def is_member(family, person_id):
    for p in family.fam:
        if p.identifier==person_id:
            return True
    return False

def find_index_by_id(options, person):
    if person is None:
        return None

    for i, candidate in enumerate(options):
        if candidate is not None and candidate.identifier == person.identifier:
            return i

    return None

# this function has the goal to update the fields, given a family and a new member:
# partner for father and mother (if both exist)
# kids for father and mother (if both exist)
# siblings (if they exist)
def upd_family_relations(family, person):
    #update partner
    if person.partner is not None:
        if person.partner.partner is None:
            person.partner.partner = person

    #update parents, siblings, and kidsg
    if person.father is not None and person.mother is not None:
        if person.father.partner is None and person.mother.partner is None:
            person.father.partner = person.mother
            person.mother.partner = person.father
        if person not in person.father.kids:
            person.father.kids.append(person)
        if person not in person.mother.kids:
            person.mother.kids.append(person)
        for s in person.father.kids: #at this point, we are assuming that father and mother have the
                                     #same kids (this entails that a person can only have one
                                     #partner)
            if s is not person and s not in person.siblings:
                person.siblings.append(s)
                s.siblings.append(person)

    return family


def delete_member(family, person):
    """
    family: Family
    person: Person
    """
    if person in family.fam:
        family.fam.remove(person)
        #family.count_id=family.count_id-1
        for p in family.fam:
            if p.father==person:
                p.father=None
            if p.mother==person:
                p.mother=None
            if person in p.kids:
                p.kids.remove(person)
            if person in p.siblings:
                p.siblings.remove(person)
            if p.partner==person:
                p.partner=None
        print(f"{person.name}({person.identifier}) was eliminated from {family.fam_name}")

    return family


def reset_family(family):
    """
    family: Family
    """
    family.fam_name = ""
    family.fam_graph=None
    family.fam=[]
    family.count=0
    family.edges=[]
    family.count_id=0
    save_family(family)


def family_from_dict(data):
    family = Family(data["fam_name"])

    family.count = data["count"]
    family.count_id = data["count_id"]

    people_data = data["fam"]
    people_by_id = {}

    # First pass: create people without relationships
    for p_data in people_data:
        health = p_data["health_info"]

        p = Person(
            name=p_data["name"],
            birth=p_data["birth"],
            death=p_data["death"],
            blood_type=health.get("blood_type", ""),
            diseases=health.get("diseases", []),
            clinical_history=health.get("clinical_history", [])
        )

        p.identifier = p_data["identifier"]
        people_by_id[p.identifier] = p

    # Second pass: reconnect relationships
    for p_data in people_data:
        p = people_by_id[p_data["identifier"]]
        if p_data["father"] is not None:
            p.father = people_by_id[p_data["father"]]
        if p_data["mother"] is not None:
            p.mother = people_by_id[p_data["mother"]]
        if p_data["partner"] is not None:
            p.partner = people_by_id[p_data["partner"]]
        p.siblings = [
            people_by_id[sibling_id]
            for sibling_id in p_data["siblings"]
        ]
        p.kids = [
            people_by_id[kid_id]
            for kid_id in p_data["kids"]
        ]
    family.fam = list(people_by_id.values())

    return family





    
