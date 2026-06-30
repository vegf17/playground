from classes import *
import json
from pathlib import Path
import os
import shutil

DATA_SOURCE = "./data/"
FAM_DIR = "fam/"
PPL_DIR = "ppl/"
PPL_FILE = "ppl.json"
COUNT_FILE = "count.json"
DIS_FILE = "dis.json"
COUNT_PATH = Path(DATA_SOURCE + COUNT_FILE)

RELATIONS=["father", "mother", "siblings", "partners", "kids"]

def start():
    data_path = Path(DATA_SOURCE)
    fam_path = Path(DATA_SOURCE + FAM_DIR)
    ppl_path = Path(DATA_SOURCE + PPL_DIR)
    if (not data_path.exists()) or (not fam_path.exists()) or (not ppl_path.exists()):
        data_path.mkdir(parents=True, exist_ok=True)
        fam_path.mkdir(parents=True, exist_ok=True)
        ppl_path.mkdir(parents=True, exist_ok=True)
        with open(Path(DATA_SOURCE+PPL_DIR+PPL_FILE), "w", encoding="utf-8") as f:
            json.dump({}, f, indent=4)
    else:
        pass        

def init_count_file():
    if COUNT_PATH.exists():
        with open(COUNT_PATH, "r") as f:
            data = json.load(f)
            count_ppl = data["count_ppl"]
            count_fam = data["count_fam"]
            print(f"count_ppl = {count_ppl}; count_fam = {count_fam}")
    else:
        count_ppl=0
        count_fam=0
        with open(COUNT_PATH, "w") as f:
            json.dump({"count_ppl": 0, "count_fam": 0}, f, indent=4)
            print(f"The file {COUNT_PATH} was created")
    #return count_ppl, count_fam


def get_count_file_info():
    if not COUNT_PATH.exists():
        count_file()

    with open(COUNT_PATH, "r") as f:
        data = json.load(f)
        count_ppl = data["count_ppl"]
        count_fam = data["count_fam"]

    return count_ppl, count_fam


def upd_count_file(count_ppl, count_fam):
    with open(COUNT_PATH, "w") as f:
        json.dump({
            "count_ppl": count_ppl,
            "count_fam": count_fam
        }, f, indent=4)
        

def init_family(name, **rest):
    """
    name: String
    **rest: remaining arguments belonging to class Family
    """

    count_ppl, count_fam = get_count_file_info()
    identifier = "f" + str(count_fam)

    file_name = identifier + "-" + name.replace(" ", "_").lower() + ".json"
    file_path = Path(DATA_SOURCE + FAM_DIR + file_name)

    members = rest["members"] if "members" in rest.keys() else []
    relations = rest["relations"] if "relations" in rest.keys() else {}

    fill_rel = fill_relations(members, relations)
    upd_relations = upd_family_relations(members, fill_rel)
    family = Family(name=name, identifier=identifier, members=members, relations=upd_relations)

    with open(file_path, "w", encoding="utf-8") as f:
        json.dump(family_to_json(family), f, indent=4)

    upd_count_file(count_ppl, count_fam+1)

    for p in members:
        upd_info_person(p, families=identifier)

    print(f"Family {name} was created!")
    return family
        

#if "relations" does not include all the members, then add the missing members
def fill_relations(members, relations):
    new_dict={}
    
    for p in members:
        new_dict[p]=fill_new_dict()
        if p in relations.keys():
            for k in relations[p].keys():
                new_dict[p][k] = relations[p][k]
    relations.clear()
    return new_dict


def add_person(name, **rest):
    """
    name: String
    **rest: birth -> String
            death -> String
            blood_type -> String
            diseases -> [String]
            photo -> Path
    """
    count_ppl, count_fam = get_count_file_info()
    identifier = "p" + str(count_ppl)

    #creates a folder for the new person
    file_name = identifier + "-" + name.replace(" ", "_").lower()
    file_path = Path(DATA_SOURCE + PPL_DIR + file_name)
    file_path.mkdir(parents=True, exist_ok=True)

    #unpacking **rest
    birth = rest["birth"] if "birth" in rest.keys() else None
    death = rest["death"] if "death" in rest.keys() else None
    blood_type = rest["blood_type"] if "blood_type" in rest.keys() else None
    diseases = rest["diseases"] if "diseases" in rest.keys() else []
    photo = rest["photo"] if "photo" in rest.keys() else None

    #creating the health_info dictionary
    health_info = {}
    health_info["blood_type"] = blood_type
    health_info["diseases"] = diseases
    
    
    #adds the person to the json file that contains all people
    ppl_path = Path(DATA_SOURCE+PPL_DIR+PPL_FILE)
    person = Person(name=name,
                    birth=birth,
                    death=death,
                    health_info=health_info,
                    photo=photo,
                    identifier=identifier
                    )
    with open(ppl_path, "r") as f:
        data = json.load(f)
        data.update(person_to_json(person))
    with open(ppl_path, "w") as f:
        json.dump(data, f, indent=4)

    #updates the count file
    upd_count_file(count_ppl+1, count_fam)

    return person

def upd_info_person(p_id, **rest):
    """
    p_id: Person identifier
    **rest: name, birth, death, blood_type, diseases, photo, families
    """
    ppl_path=Path(DATA_SOURCE+PPL_DIR+PPL_FILE)
    with open(ppl_path, "r") as f:
        data = json.load(f)
        for k,v in rest.items():
            if k=="families" and v not in data[p_id][k]:
                data[p_id][k].append(v)
            elif k=="blood_type" or k=="diseases":
                data[p_id]["health_info"].update({k:v})
            else:
                data[p_id][k]=v
    with open(ppl_path, "w") as f:
        json.dump(data, f, indent=4)


def rmv_person(p_id):
    ppl_path=Path(DATA_SOURCE+PPL_DIR+PPL_FILE)
    with open(ppl_path, "r") as f:
        data = json.load(f)
        for fam_id in data[p_id]["families"]:
            rmv_person_family(fam_id, p_id) 
        data.pop(p_id)
    with open(ppl_path, "w") as f:
        json.dump(data, f, indent=4)

def rmv_person_family(fam_id, p_id):
    fam_path = Path(DATA_SOURCE+FAM_DIR)
    list_files = [f for f in os.listdir(fam_path) if os.path.isfile(DATA_SOURCE+FAM_DIR+f) and (fam_id+'-') in f]
    selected_file = list_files[0]
    file_path = Path(DATA_SOURCE+FAM_DIR+selected_file)
    with open(file_path, "r") as f:
        data = json.load(f)
        members=data[fam_id]["members"]
        members.remove(p_id)
        upd_relations = rmv_family_relations(p_id, data[fam_id]["relations"])
        data[fam_id]["relations"] = upd_relations
    with open(file_path, "w") as f:
        json.dump(data, f, indent=4)    
        
def add_person_to_family(fam_id, p, **relations):
    """
    fam_id: String
    p: Person
    **relations: father, mother, siblings, partners, kids
    """
    upd_info_person(p.identifier, families=fam_id)
    fam_path = Path(DATA_SOURCE+FAM_DIR)
    list_files = [f for f in os.listdir(fam_path) if os.path.isfile(DATA_SOURCE+FAM_DIR+f) and (fam_id+'-') in f]
    selected_file = list_files[0]
    file_path = Path(DATA_SOURCE+FAM_DIR+selected_file)
    with open(file_path, "r") as f:
        data = json.load(f)
        members=data[fam_id]["members"]
        members.append(p.identifier)
        data[fam_id]["relations"][p.identifier] = fill_relations_member(relations)
        upd_relations = upd_family_relations(members, data[fam_id]["relations"])
        data[fam_id]["relations"] = upd_relations
    with open(file_path, "w") as f:
        json.dump(data, f, indent=4)

#see if it is possible to simplify some of the coding that we have here
def upd_family_relations(members, relations):
    """
    members: members of the family
    relations: dictionary of relations
    """
    keys = relations.keys()
    for p in keys:
        #if father exists and it is a member of the family
        if relations[p]["father"] and relations[p]["father"] in members:
            father=relations[p]["father"]
            #if the code enters here, it means that p has a father
            #(a)for every kid the father has do:
            #(1) connect siblings
            #(2) if the kids have no father, then add father to the kids
            #(3) add p to father kids'
            #
            #(b)if father has only one partner do:
            #(1) share the kids with the partner
            #(2) update the mother status
            for k in relations[father]["kids"]:#(a)
                if k not in relations[p]["siblings"] and k!=p: #(1)
                    relations[p]["siblings"].append(k)
                if p not in relations[k]["siblings"] and k!=p:
                    relations[k]["siblings"].append(p)
                if not relations[k]["father"]: #(2)
                    relations[k]["father"]=father
            if p not in relations[father]["kids"]: #(3)
                relations[father]["kids"].append(p)
            if len(relations[father]["partners"])==1:#(b)
                partner = relations[father]["partners"][0]
                for k in relations[father]["kids"]: #(1)
                    if k not in relations[partner]["kids"]:
                        relations[partner]["kids"].append(k)
                    if not relations[k]["mother"]: #(2)
                        relations[k]["mother"]=partner

                
                                    
        #if mother exists and it is a member of the family
        if relations[p]["mother"] and relations[p]["mother"] in members:
            mother=relations[p]["mother"]            
            #if the code enters here, it means that p has a mother
            #for every kid the mother has do:
            #(1) connect siblings
            #(2) if the kids have no mother, then add mother to the kids
            #(3) add p to mother kids'
            #
            #(b)if father has only one partner do:
            #(1) share the kids with the partner
            #(2) update the mother status
            for k in relations[mother]["kids"]:
                if k not in relations[p]["siblings"] and k!=p: #(1)
                    relations[p]["siblings"].append(k)
                if p not in relations[k]["siblings"] and k!=p:
                    relations[k]["siblings"].append(p)
                if not relations[k]["mother"]: #(2)
                    relations[k]["mother"]=mother
            if p not in relations[mother]["kids"]: #(3)
                relations[mother]["kids"].append(p)
            if len(relations[mother]["partners"])==1:#(b)
                partner = relations[mother]["partners"][0]
                for k in relations[mother]["kids"]: #(1)
                    if k not in relations[partner]["kids"]:
                        relations[partner]["kids"].append(k)
                    if not relations[k]["father"]: #(2)
                        relations[k]["father"]=partner                


        #for each partner do:
        #if the partner has not p in partners, then add p to partner partners
        for r in relations[p]["partners"]:
            if p not in relations[r]["partners"] and p!=r:
                relations[r]["partners"].append(p)

        #if p has only one partner do:
        #
        #(1) if p has kids, then add those kids to partner kids and update the mother and father
        #fields if necessary
        if len(relations[p]["partners"])==1:
            partner = relations[p]["partners"][0]
            for k in relations[p]["kids"]:
                if k not in relations[partner]["kids"]:
                    relations[partner]["kids"].append(k)


        #at this point, if p is a kid of someone, then the fields father and mother are filled
        #here we will redo the siblings
        #if father and mother exist and belong to the family do:
        #
        #(1) if father does not belong to mother partners' or mother does not belong to father
        #partners' then add father to mother partners' and mother to father partners', respectively
        #
        #(2) for every father partners and every mother partners, update the siblings relation
        #print(f"before loop (father,mother) {relations[p]["father"], relations[p]["mother"]}")
        if relations[p]["father"] and relations[p]["mother"] and relations[p]["father"] in members and relations[p]["mother"] in members:
            father = relations[p]["father"]
            mother = relations[p]["mother"]
            # if mother not in relations[father]["partners"]: #(1)
            #     relations[father]["partners"].append(mother)
            # if father not in relations[mother]["partners"]: #(1)
            #     relations[mother]["partners"].append(father)
            for partner in relations[father]["partners"]: #(2)
                #print(f"\tfather partner: {partner}")
                for k in relations[partner]["kids"]:
                    #print(f"\t\tfather partner kid: {k}")
                    if relations[k]["father"]==father:
                        for kk in relations[father]["kids"]:
                            #print(f"\t\t\tfather kid: {kk}")
                            if kk not in relations[k]["siblings"] and k!=kk:
                                relations[k]["siblings"].append(kk)
                            if k not in relations[kk]["siblings"] and k!=kk:
                                relations[kk]["siblings"].append(k)
            for partner in relations[mother]["partners"]: #(2)
                #print(f"\tmother partner: {partner}")
                for k in relations[partner]["kids"]:
                    #print(f"\t\tmother partner kid: {k}")
                    if relations[k]["mother"]==mother:
                        for kk in relations[mother]["kids"]:
                            #print(f"\t\t\tmother kid: {kk}")
                            if kk not in relations[k]["siblings"] and k!=kk:
                                relations[k]["siblings"].append(kk)
                            if k not in relations[kk]["siblings"] and k!=kk:
                                relations[kk]["siblings"].append(k)                            
                            
    return relations


def rmv_family_relations(p_id, relations):
    rel_id = relations[p_id]
    if rel_id["father"]:
        relations[rel_id["father"]]["kids"].remove(p_id)
    if rel_id["mother"]:
        relations[rel_id["mother"]]["kids"].remove(p_id)
    for s in rel_id["siblings"]:
        relations[s]["siblings"].remove(p_id)
    for p in rel_id["partners"]:
        for k in relations[p]["kids"]:
            for kk in rel_id["kids"]:
                if relations[k]["father"]!=relations[kk]["father"] or relations[k]["mother"]!=relations[kk]["mother"]:
                    if kk in relations[k]["siblings"]:
                        relations[k]["siblings"].remove(kk)
                    if k in relations[kk]["siblings"]:
                        relations[kk]["siblings"].remove(k)
        relations[p]["partners"].remove(p_id)
    for k in rel_id["kids"]:
        if p_id==relations[k]["father"]:
            relations[k]["father"]=None
        else:
            relations[k]["mother"]=None
    relations.pop(p_id)
    return relations


def fill_relations_member(relations):
    new_dict = fill_new_dict()
    for k in relations.keys():
        new_dict[k] = relations[k]
    relations.clear()

    return new_dict


def fill_new_dict():
    new_dict={}
    for k in RELATIONS:
        if k=="father" or k=="mother":
            new_dict[k]=None
        else:
            new_dict[k]=[]
    return new_dict
        


# #given a person identifier, returns a dictionary where keys are the persons_id (until the 3rd-degree
# #relatives) and the values are a list of diseases
def track_person_diseases(p_id):
    # find a person given a person identifier
    p = get_person_by_id(p_id)

    if p is None:
        print(f"It does not exist a person with identifier: {p_id}")
        return {}

    track_diseases = {}

    def add_relative(relative_id, fam_name, degree, relation_name):
        """
        Helper function to avoid repeating the same code many times.
        """
        if relative_id is None:
            return

        relative = get_person_by_id(relative_id)

        if relative is None:
            return

        diseases = relative.get("health_info", {}).get("diseases", [])

        track_diseases[relative_id] = {
            "fam": fam_name,
            "disease": diseases,
            "degree": degree,
            "relation": relation_name
        }

    for fam_id in p["families"]:
        fam_path = Path(DATA_SOURCE + FAM_DIR)

        list_files = [
            f for f in os.listdir(fam_path)
            if os.path.isfile(DATA_SOURCE + FAM_DIR + f) and (fam_id + '-') in f
        ]

        if not list_files:
            continue

        selected_file = list_files[0]
        file_path = Path(DATA_SOURCE + FAM_DIR + selected_file)

        with open(file_path, "r") as f:
            fam = json.load(f)

        fam_name = fam[fam_id]["name"]
        relations = fam[fam_id]["relations"]

        if p_id not in relations:
            continue

        p_rel = relations[p_id]

        father_id = p_rel.get("father")
        mother_id = p_rel.get("mother")

        # 1st-degree relatives: parents, biological siblings, kids

        # father
        add_relative(father_id, fam_name, 1, "Father")

        # mother
        add_relative(mother_id, fam_name, 1, "Mother")

        # biological siblings
        sibl_id = biological_siblings(
            p_id,
            father_id,
            mother_id,
            relations
        )

        for s_id in sibl_id:
            add_relative(s_id, fam_name, 1, "Sibling")

        # kids
        for k_id in p_rel.get("kids", []):
            add_relative(k_id, fam_name, 1, "Child")

        # 2nd-degree relatives:
        # grandparents, aunts, uncles, nieces, nephews, half-siblings

        grandparents = []

        # paternal grandparents
        if father_id is not None and father_id in relations:
            gfa_fa_id = relations[father_id].get("father")
            gmo_fa_id = relations[father_id].get("mother")

            grandparents.append((gfa_fa_id, "Paternal Grandfather"))
            grandparents.append((gmo_fa_id, "Paternal Grandmother"))

            aunts_uncles_fa_id = relations[father_id].get("siblings", [])
        else:
            aunts_uncles_fa_id = []

        # maternal grandparents
        if mother_id is not None and mother_id in relations:
            gfa_mo_id = relations[mother_id].get("father")
            gmo_mo_id = relations[mother_id].get("mother")

            grandparents.append((gfa_mo_id, "Maternal Grandfather"))
            grandparents.append((gmo_mo_id, "Maternal Grandmother"))

            aunts_uncles_mo_id = relations[mother_id].get("siblings", [])
        else:
            aunts_uncles_mo_id = []

        # grandparents
        for gp_id, relation_name in grandparents:
            add_relative(gp_id, fam_name, 2, relation_name)

        # aunts and uncles
        for au_id in aunts_uncles_fa_id:
            add_relative(au_id, fam_name, 2, "Paternal Aunt/Uncle")

        for au_id in aunts_uncles_mo_id:
            add_relative(au_id, fam_name, 2, "Maternal Aunt/Uncle")

        # nieces and nephews
        niblings_id = [
            kid
            for s in sibl_id
            for kid in relations.get(s, {}).get("kids", [])
        ]

        for n_id in niblings_id:
            add_relative(n_id, fam_name, 2, "Niece/Nephew")

        # half-siblings
        half_siblings_id = [
            s
            for s in p_rel.get("siblings", [])
            if s not in sibl_id
        ]

        for hs_id in half_siblings_id:
            add_relative(hs_id, fam_name, 2, "Half-sibling")

        # 3rd-degree relatives:
        # first cousins, great-grandparents, half-niblings

        great_grandparents = []

        for gp_id, gp_relation_name in grandparents:
            if gp_id is not None and gp_id in relations:
                ggf_id = relations[gp_id].get("father")
                ggm_id = relations[gp_id].get("mother")

                great_grandparents.append(
                    (ggf_id, f"Great-grandfather through {gp_relation_name}")
                )
                great_grandparents.append(
                    (ggm_id, f"Great-grandmother through {gp_relation_name}")
                )

        for ggp_id, relation_name in great_grandparents:
            add_relative(ggp_id, fam_name, 3, relation_name)

        # first cousins
        cousins_id = [
            kid
            for e in aunts_uncles_fa_id + aunts_uncles_mo_id
            for kid in relations.get(e, {}).get("kids", [])
        ]

        for c_id in cousins_id:
            add_relative(c_id, fam_name, 3, "First cousin")

        # half-niblings: children of half-siblings
        half_niblings_id = [
            kid
            for s in half_siblings_id
            for kid in relations.get(s, {}).get("kids", [])
        ]

        for hn_id in half_niblings_id:
            add_relative(hn_id, fam_name, 3, "Half-niece/Half-nephew")

    return track_diseases
        
    

def biological_siblings(p_id, father_id, mother_id, relations):
    bio_sib = []
    for s in relations[p_id]["siblings"]:
        if relations[s]["father"]==father_id and relations[s]["mother"]==mother_id:
            bio_sib.append(s)
    return bio_sib
        
def get_person_by_id(p_id):
    ppl_path = Path(DATA_SOURCE+PPL_DIR+PPL_FILE)
    if ppl_path.exists():
        with open(ppl_path, "r") as f:
            data = json.load(f)
            try:
                data[p_id]
            except KeyError:
                print(f"It does not exist the person with identifier: {p_id}")
                return None
            else:
                p = data[p_id]
                return p
    else:
        print("The file with people was not created")
        return None


#given a family identifier, returns a dictionary whose keys are the different blood types and the values are a
#list of person that have that blood type
def fam_blood_types(fam_id):
    fam_path = Path(DATA_SOURCE+FAM_DIR)
    list_files = [f for f in os.listdir(fam_path) if os.path.isfile(DATA_SOURCE+FAM_DIR+f) and (fam_id+'-') in f]
    selected_file = list_files[0]
    file_path = Path(DATA_SOURCE+FAM_DIR+selected_file)
    with open(file_path, "r") as f:
        data = json.load(f)
        dict_blood_types = {}
        for p_id in data[fam_id]["members"]:
            p = get_person_by_id(p_id)
            p_name = p["name"]
            p_blood_type = p["health_info"]["blood_type"]
            if p_blood_type not in dict_blood_types.keys():
                dict_blood_types[p_blood_type] = [(p_name, p_id)]
            else:
                dict_blood_types[p_blood_type].append((p_name, p_id))
    return dict_blood_types

#given a family identifier, returns a dictionary whose keys are the different diseases and the values are a
#list of person that have that disease
def fam_diseases(fam_id):
    fam_path = Path(DATA_SOURCE+FAM_DIR)
    list_files = [f for f in os.listdir(fam_path) if os.path.isfile(DATA_SOURCE+FAM_DIR+f) and (fam_id+'-') in f]
    selected_file = list_files[0]
    file_path = Path(DATA_SOURCE+FAM_DIR+selected_file)
    with open(file_path, "r") as f:
        data = json.load(f)
        diseases_fam = {}
        for p_id in data[fam_id]["members"]:
            p = get_person_by_id(p_id)
            p_name = p["name"]
            p_diseases = p["health_info"]["diseases"]
            for dis in p_diseases:
                if dis not in diseases_fam.keys():
                    diseases_fam[dis] = [(p_name, p_id)]
                else:
                    diseases_fam[dis].append((p_name, p_id))
    return diseases_fam
