from backend import *
from classes import *
import shutil
from sample_family_data import PERSON_SPECS, FAMILY_MEMBERS, FAMILY_NAMES, PARENT_LINKS, PARTNER_LINKS
from seed_sample_data import seed_sample_data

shutil.rmtree(DATA_SOURCE)
start()
init_count_file()

p0=add_person("Manuel Fernandes")
p1=add_person("Maria Fernandes")
p2=add_person("Vitor Fernandes")
p3=add_person("David Fernandes")
p4=add_person("Carlos Fernandes")
p5=add_person("Gabriela Antonieta")
p6=add_person("Maria Antonieta")
p7=add_person("Jose Afonso")
p8=add_person("Antonio Carlos")
p9=add_person("Josefina Antunes")
p10=add_person("Cristo Rei")


f0=init_family("Fernandes",
               members=[p0.identifier, p1.identifier, p2.identifier],
               relations={
                   p0.identifier : {
                       "partners" : [p1.identifier],
                       "kids": [p2.identifier]
                   }
               })

add_person_to_family(f0.identifier, p3, father=p0.identifier)
add_person_to_family(f0.identifier, p4, mother=p1.identifier)
add_person_to_family(f0.identifier, p5, partners=[p0.identifier])
add_person_to_family(f0.identifier, p6, mother=p5.identifier)
add_person_to_family(f0.identifier, p7, partners=[p5.identifier])
add_person_to_family(f0.identifier, p8, father=p7.identifier, partners=[p3.identifier])
add_person_to_family(f0.identifier, p9, partners=[p2.identifier])
add_person_to_family(f0.identifier, p10, mother=p9.identifier)

upd_info_person(p0.identifier, blood_type="A+", diseases=["diabetes", "asthma"])
upd_info_person(p1.identifier, blood_type="A-", diseases=[])
upd_info_person(p2.identifier, blood_type="B+", diseases=["hypertension"])
upd_info_person(p3.identifier, blood_type="B-", diseases=["asthma", "eczema"])
upd_info_person(p4.identifier, blood_type="O+", diseases=["diabetes", "hypertension", "arthritis"])
upd_info_person(p5.identifier, blood_type="O-", diseases=[])
upd_info_person(p6.identifier, blood_type="AB+", diseases=["migraine"])
upd_info_person(p7.identifier, blood_type="AB-", diseases=["asthma", "migraine"])
upd_info_person(p8.identifier, blood_type="A+", diseases=["eczema"])
upd_info_person(p9.identifier, blood_type="A-", diseases=[])
upd_info_person(p10.identifier, blood_type="B+", diseases=["diabetes", "arthritis"])


seed_sample_data()

print(track_person_diseases("p17"))

# print(track_person_diseases(p2.identifier))
# print(fam_blood_types(f0.identifier))
# print(fam_diseases(f0.identifier))



# f1=init_family("Silva")

# add_person_to_family(f1.identifier, p7)
# add_person_to_family(f1.identifier, p9)

# #rmv_person(p9.identifier)
