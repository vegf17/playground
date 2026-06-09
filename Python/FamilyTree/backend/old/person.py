import graphviz as gvz
import json

class Family:
    def __init__(self,
                 fam_name="",
                 #fam_graph=None,
                 fam=None,
                 count=0,
                 edges=None,
                 count_id=0,
                 family_id=None):
        self.fam_name = fam_name
        #self.fam_graph = gvz.Digraph(comment=fam_name)
        self.fam = fam if fam is not None else []
        self.count = count #current couples in the family
        self.edges = edges if edges is not None else []
        self.count_id = count_id #next number for the identifier
        self.family_id = family_id #family identifier (useful to create folders)
    
    def printFamily(self):
        print("Family: " + self.fam_name + "\n")
        for p in self.fam:
            p.printPerson()

    def create_dict(self):
        return {
            "fam_name" : self.fam_name,
            "family_id" : self.family_id,
            "count": self.count,
            "count_id": self.count_id,
            "fam" : [person_to_dict(self.fam, p) for p in self.fam]
        }

    def toJSON(self):
        return json.dumps(self.create_dict())
        

class Person:
    def __init__(self,
                 name,
                 birth,
                 death="",
                 blood_type="",
                 diseases=None,
                 clinical_history=None,
                 father=None, #Person (in JSON it is saved the identifier)
                 mother=None, #Person
                 siblings=None, #[Person]
                 partners=None, #[Person]
                 kids=None, #[Person]
                 identifier=None, #string
                 photo=None #location 
                 ):
        self.name = name
        self.birth = birth
        self.death = death
        self.health_info = {
            "blood_type": blood_type,
            "diseases": diseases if diseases is not None else [],
            "clinical_history": clinical_history if clinical_history is not None else []
        }
        self.father = father
        self.mother = mother
        self.siblings = siblings if siblings is not None else []
        self.partners = partners if partners is not None else []
        self.kids = kids if kids is not None else []
        self.identifier = identifier
        self.photo = photo

    def printPerson(self):
        print("Name: %s" % self.name)
        print("Birthday: %s" % self.birth)
        if self.death: #if self death is not equal to ""
            print("Death: %s" % self.death)

        if self.father or self.mother or self.partners or self.siblings or self.kids is not None:
            print("Family:")

            if self.father is not None:
                print("Father:", self.father.name)
            if self.mother is not None:
                print("Mother:", self.mother.name)
            if self.partners is not None:
                print("Partner(s):", [partner.name for partner in self.partners])
            if self.siblings:
                print("Siblings:", [sibling.name for sibling in self.siblings])
            if self.kids:
                print("Kids:", [kid.name for kid in self.kids])
            
        print("Blood type: %s" % self.health_info.get("blood_type"))
        if self.health_info.get("diseases"): # if self.health_info.get("diseases" is not empty
            print("Diseases:")
            for i in self.health_info.get("diseases"):
                print("\t%s" % i)
        else:
            print("Diseases: None")
        if self.health_info.get("clinical_history"):
            print("Clinical history:")
            for i in self.health_info.get("clinical_history"):
                print("Date: %s" %i[0])
                for p in i[1]:
                    print("\t%s" % p)
        print("\n")
    

def person_to_dict(fam, p):
    """
    fam: [Person]
    p: Person
    """
    return {
        "photo": p.photo,
        "identifier": p.identifier,
        "name": p.name,
        "birth": p.birth,
        "death": p.death,
        "health_info": p.health_info,
        "father": p.father.identifier if p.father is not None else None,
        "mother": p.mother.identifier if p.mother is not None else None,
        "siblings": [sibling.identifier for sibling in p.siblings],
        "partners": [partner.identifier for partner in p.partners],
        "kids": [kid.identifier for kid in p.kids]
    }

    


