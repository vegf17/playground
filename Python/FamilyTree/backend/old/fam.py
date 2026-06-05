from person import Person, Family
from pathlib import Path
import json
import shutil
#import streamlit as st
import graphviz as gvz

#DATA_DIR = Path("./data/families")
#DATA_DIR.mkdir(parents=True, exist_ok=True)

DATA_SOURCE = "./data/"
count_file = "count.json"
count_path = Path(DATA_SOURCE + count_file)

def count_family():
    if count_path.exists():
        with open(count_path, "r") as f:
            data = json.load(f)
            count = data["count"]
            print(f"The value of count is: {count}")
    else:
        count=0
        with open(count_path, "w") as f:
            json.dump({"count": 0}, f, indent=4)
            print(f"The file {count_path} was created")
    return count


def init_family(fam_name):
    """
    fam_name: String
    """
    count = count_family()
    family_id = "f" + str(count)
    
    folder_name = fam_name.replace(" ", "_").lower() + "-" + str(family_id)
    DATA_DIR = Path(DATA_SOURCE+folder_name)
    DATA_DIR.mkdir(parents=True, exist_ok=True)

    with open(count_path, "w") as f:
        json.dump({"count": count+1}, f, indent=4)
    
    return Family(fam_name, family_id=family_id)


def save_family(family):
    """
    family: Family
    """
    folder_name = family.fam_name.replace(" ", "_").lower() + "-" + str(family.family_id) + "/"
    filename = family.fam_name.replace(" ", "_").lower() + "-" + str(family.family_id) + ".json"
    path = DATA_SOURCE + folder_name + filename

    with open(path, "w", encoding="utf-8") as f:
        json.dump(family.create_dict(), f, indent=4, ensure_ascii=False)
    print(family.fam_name + " was created in " + f"{path}")


def load_family(fam_name, family_id):
    """
    fam_name: Family name or json family name
    family_id: Identifier of the family
    """
    try:
        f_name, f_id = fam_name.rsplit("-")
    except ValueError:
        f_name = fam_name
        f_id = ""
    if f_id == "":
        folder_name = fam_name.replace(" ", "_").lower() + "-" + str(family_id) + "/"
        filename = fam_name.replace(" ", "_").lower() + "-" + str(family_id) + ".json"
    else:
        folder_name = fam_name.replace(" ", "_").lower() + "/"
        filename = fam_name.replace(" ", "_").lower() + ".json"
    path = DATA_SOURCE + folder_name + filename

    with open(path, "r") as f:
        data = json.load(f)

    return family_from_dict(data)


####updated this function to support more than one partner
def add_new_member(family, person):
    """
    family: Family
    person: Person
    """
    person.identifier = "p"+f"{family.count_id}"
    family.count_id = family.count_id+1
    family.fam.append(person)

    #if the added person selects a partner, which is already in the family, then it updates the
    #information of the partner by adding the added person as a partner
    for partner in person.partners:
        if partner in family.fam and person not in partner.partners:
            partner.partners.append(person)

    save_family(family)
    #family.fam_graph.node(f"{family.fam.index(person)}", person.name)

    #creates a folder, inside the family folder, for the added person
    family_folder_name = family.fam_name.replace(" ", "_").lower() + "-" + str(family.family_id) + "/"
    person_folder_name = person.name.replace(" ", "_").lower() + "-" + person.identifier
    path = DATA_SOURCE + family_folder_name + person_folder_name
    DATA_DIR = Path(path)
    DATA_DIR.mkdir(parents=True, exist_ok=True)

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
                    partners,
                    photo
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
    p.partners = partners
    p.photo = photo

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

####updated this function to support more than one partner
# this function has the goal to update the connections between family members, given a family and a
# new member:
#
# partner for father and mother (if both exist)
# kids for father and mother (if both exist)
# siblings (if they exist)
def upd_family_relations(family, person):
    #update partners
    for partner in person.partners:
        if person not in partner.partners:
            partner.partners.append(person)
    # if person.partner is not None:
    #     if person.partner.partner is None:
    #         person.partner.partner = person

    #update parents, siblings, and kids
    if person.father is not None and person.mother is not None:
        for partner in person.father.partners:
            if partner.identifier==person.mother.identifier and person.mother not in person.father.partners:
                person.father.partners.append(person.mother)
                person.mother.partners.append(person.father)
        for partner in person.mother.partners:
            if partner.identifier==person.father.identifier and person.father not in person.mother.partners:
                person.father.partners.append(person.mother)
                person.mother.partners.append(person.father)
        if person not in person.father.kids:
            person.father.kids.append(person)
        if person not in person.mother.kids:
            person.mother.kids.append(person)
        for s in person.father.kids: 
            if s is not person and s not in person.siblings:
                person.siblings.append(s)
                s.siblings.append(person)
        for s in person.mother.kids:
            if s is not person and s not in person.siblings:
                person.siblings.append(s)
                s.siblings.append(person)
            

    # #update parents, siblings, and kids
    # if person.father is not None and person.mother is not None:
    #     if person.father.partner is None and person.mother.partner is None:
    #         person.father.partner = person.mother
    #         person.mother.partner = person.father
    #     if person not in person.father.kids:
    #         person.father.kids.append(person)
    #     if person not in person.mother.kids:
    #         person.mother.kids.append(person)
    #     for s in person.father.kids: #at this point, we are assuming that father and mother have the
    #                                  #same kids (this entails that a person can only have one
    #                                  #partner)
    #         if s is not person and s not in person.siblings:
    #             person.siblings.append(s)
    #             s.siblings.append(person)

    return family


####updated this function to support more than one partner
def delete_member(family, person):
    """
    family: Family
    person: Person
    """

    #removes all the information associated with the person
    family_folder_name = family.fam_name.replace(" ", "_").lower() + "-" + str(family.family_id) + "/"
    person_folder_name = person.name.replace(" ", "_").lower() + "-" + person.identifier
    path = DATA_SOURCE + family_folder_name + person_folder_name
    DATA_DIR = Path(path)
    shutil.rmtree(DATA_DIR)

    #removes the person from the family, as well as it updates the information of the family members
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
            if person in p.partners:
                p.partners.remove(person)
        print(f"{person.name}({person.identifier}) was eliminated from {family.fam_name}")

    return family


def delete_family_folder(family):
    """
    family: Family
    """
    
    #removes all the information associated with a family
    family_folder_name = family.fam_name.replace(" ", "_").lower() + "-" + str(family.family_id)
    path = DATA_SOURCE + family_folder_name
    DATA_DIR = Path(path)
    shutil.rmtree(DATA_DIR)


def reset_family(family):
    """
    family: Family
    """
    family.fam_name = ""
    #family.fam_graph=None
    family.fam=[]
    family.count=0
    family.edges=[]
    family.count_id=0
    save_family(family)


####updated this function to support more than one partner
def family_from_dict(data):
    family = Family(data["fam_name"])

    family.family_id = data["family_id"]
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
        p.photo = p_data["photo"]
        people_by_id[p.identifier] = p

    # Second pass: reconnect relationships
    for p_data in people_data:
        p = people_by_id[p_data["identifier"]]
        if p_data["father"] is not None:
            p.father = people_by_id[p_data["father"]]
        if p_data["mother"] is not None:
            p.mother = people_by_id[p_data["mother"]]
        # if p_data["partner"] is not None:
        #     p.partner = people_by_id[p_data["partner"]]
        p.partners = [people_by_id[partner_id] for partner_id in p_data["partners"]]
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




#different forms to display the graph of a family
#1) Graphviz
def create_graph_fam(fam_graph, edges, fam, count):
    """
    fam_graph: DiGraph
    edges: [(node_id, node_id)]
    fam: [Person]
    count: int
    """
    fam_graph = node_fam(fam_graph, fam) #nodes
    fam_graph, edges, count = edge_fam(fam_graph, edges, fam, count) #edges

    return fam_graph, edges, count

def node_fam(fam_graph, fam):
    """
    fam_graph: Digraph 
    fam: list of Person; [Person]
    """
    for person in fam:
        fam_graph.node(person.identifier, person.name)

    return fam_graph


####updated this function to support more than one partner - to do
def edge_fam(fam_graph, edges, fam, i):
    """
    fam_graph: Digraph
    edges: [(node_id, node_id)]
    fam: list of Person; [Person]
    i: Int (for naming possible invisible nodes)
    """
    for person in fam:
        person_id = person.identifier
        edges_fst = [x for (x,y) in edges]
        edges_snd = [y for (x,y) in edges]

        for partner in person.partners:
            if partner in fam:
                partner_id = partner.identifier
                if (partner_id in edges_fst) or (partner_id in edges_snd):
                    pass
                else:
                    (i, edges) = add_invisible_node(fam_graph, fam, person, partner, i+1, edges)        
        # if person.partner is not None and person.partner in fam:
        #     partner_id = person.partner.identifier #f"{fam.index(person.partner)}"
        #     if (partner_id in edges_fst) or (partner_id in edges_snd):
        #         pass
        #     else:
        #         (i, edges) = add_invisible_node(fam_graph, fam, person, person.partner, i+1, edges)        

        if person.father is not None and person.mother is not None and person.father in fam and person.mother in fam:
            father_id = person.father.identifier #f"{fam.index(person.father)}"
            mother_id = person.mother.identifier #f"{fam.index(person.mother)}"
            if is_pair_with_inv_node(father_id, edges)==False:
                (i, edges) = add_invisible_node(fam_graph, fam, person.father, person.mother, i+1, edges)
            inv_node_id = get_inv_node_id(father_id, edges)
            fam_graph.edge(inv_node_id, person_id)
            edges.append((inv_node_id, person_id))

    return fam_graph, edges, i

def get_inv_node_id(n_id, edges):
    """
    n_id: id of the n-th node
    edges: list of edges
    """
    for (x,y) in edges:
        if x==n_id: #inv nodes are always on the RHS of an edge
            return y    

def is_pair_with_inv_node(n_id, edges):
    """
    n_id: id of the n-th node
    edges: list of edges
    """
    for (x,y) in edges:
        if x==n_id: #inv nodes are always on the RHS of an edge
            return "inv" in y

    return False

def add_invisible_node(fam_graph, fam, person, person_partner, i, edges):
    """
    fam_graph: Digraph
    fam: [Person]
    person, person_partner: Person
    i: int (used to name invisible nodes)
    edges: list of edges
    """
    inv_id = "inv"+f"{i}"
    person_id = person.identifier #f"{fam.index(person)}"
    partner_id = person_partner.identifier #f"{fam.index(person_partner)}"
    fam_graph.node(inv_id, shape="point")
    fam_graph.edge(person_id, inv_id)
    fam_graph.edge(partner_id, inv_id)
    edges.append((person_id, inv_id))
    edges.append((partner_id, inv_id))

    return (i, edges)
        

#display graph using instructions in the command line
def showGraph(family):
    fam_graph = gvz.Digraph(comment=family.fam_name)
    create_graph_fam(fam_graph, [], family.fam, 0)
    fam_graph.render("doctest-output/"+family.fam_name+".gv", view=True)

#display graph in streamlit
def showGraphSt(family):
    fam_graph = gvz.Digraph(comment=family.fam_name)
    create_graph_fam(fam_graph, [], family.fam, 0)

    return fam_graph
