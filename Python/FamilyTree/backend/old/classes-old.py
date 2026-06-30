import json

class Person:
    def __init__(self,
                name, #String
                birth=None, #String
                death=None, #String #in the future evolve to (Date, Cause of death)
                health_info=None, #Dictionary #in the future, in the diseases, also take into account the year of diagnosis
                photo=None, #Path to a picture
                identifier=None, #String: p ++ number
                families=None #list of families the person belongs
                ):
        self.name = name
        self.birth = birth if birth is not None else ""
        self.death = death if death is not None else ""
        self.health_info = health_info if health_info is not None else {}
        self.photo = photo if photo is not None else ""
        self.identifier = identifier if identifier is not None else ""
        self.families = families if families is not None else []


class Family:
    def __init__(self,
                 name, #String
                 identifier=None, #String: f ++ number
                 members=None, #list of Person.identifier
                 relations=None #dictionary of a dictionary
                 ):
        self.name = name
        self.identifier = identifier if identifier is not None else ""
        self.members = members if members is not None else []
        self.relations = relations if relations is not None else {}


def person_to_json(p):
    return{
        str(p.identifier): {
            "name": p.name,
            "families": p.families,
            "photo": p.photo,
            "birth": p.birth,
            "death": p.death,
            "health_info": p.health_info
        }
    }

def family_to_json(fam):
    return{
        str(fam.identifier): {
            "name": fam.name,
            "members": [p for p in fam.members],
            "relations": fam.relations
        }
    }
