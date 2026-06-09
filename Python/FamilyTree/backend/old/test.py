from person import *
import networkx as nx
import matplotlib.pyplot as plt
from fam import *

import graphviz
from graphviz import Digraph

# #DEFINITION OF FAM1
# fam1 = init_family("Family 1")

# p1 = Person("Father", "01/01/1965")
# p2 = Person("Mother", "01/01/1968")
# p3 = Person("Sibling One", "01/01/1994", father=p1, mother=p2)
# p4 = Person("Sibling Two", "01/01/1998", father=p1, mother=p2)
# p5 = Person("Partner One", "01/01/1996")
# p6 = Person("Partner Two", "01/01/1997")
# p7 = Person("Kid One", "01/01/2020", father=p5, mother=p3)
# p8 = Person("Kid Two", "01/01/2021", father=p6, mother=p4)
# p9 = Person("Kid Three", "01/01/2022", father=p6, mother=p4)
# p10 = Person("Kid Four", "01/01/2023", father=p6, mother=p4)

# p1.partner = p2
# p2.partner = p1

# p1.kids = [p3, p4]
# p2.kids = [p3, p4]

# p3.siblings = [p4]
# p4.siblings = [p3]

# p3.partner = p5
# p5.partner = p3

# p4.partner = p6
# p6.partner = p4

# p3.kids = [p7]
# p5.kids = [p7]

# p4.kids = [p8, p9, p10]
# p6.kids = [p8, p9, p10]

# fam1_list = [p1,p2,p3,p4,p5,p6,p7,p8,p9,p10]

# for p in fam1_list:
#     add_new_member(fam1,p)

# #fam1.showGraph()

# #DEFINITION OF FAM2
# fam2 = init_family("Family 2")
# q1 = Person("Father", "01/01/1965")
# q2 = Person("Mother", "01/01/1968")
# q3 = Person("Sibling One", "01/01/1994", father=q1, mother=q2)
# q4 = Person("Sibling Two", "01/01/1998", father=q1, mother=q2)
# q5 = Person("Partner One", "01/01/1996")
# q6 = Person("Kid One", "01/01/2020", father=q5, mother=q3)


# q1.partner = q2
# q2.partner = q1

# q1.kids = [q3, q4]
# q2.kids = [q3, q4]

# q3.siblings = [q4]
# q4.siblings = [q3]

# q3.partner = q5
# q5.partner = q3

# q3.kids = [q6]
# q5.kids = [q6]

# fam2_list = [q1,q2,q3,q4,q5,q6]

# for p in fam2_list:
#     add_new_member(fam2,p)

# #fam2.showGraph()

#DEFINITION OF FAMILY FAM3
try:
    fam3 = load_family("Family 3", "f5")
    delete_family_folder(fam3)
    fam3 = init_family("Family 3")
except FileNotFoundError:
    fam3 = init_family("Family 3")
    
pp1 = Person("Antonio", "01/01/1990")
pp2 = Person("Maria", "01/01/1990")
pp3 = Person("Antonia", "02/02/1990")
pp4 = Person("Jose", "03/03/2000")

pp1.partners.append(pp2)
pp2.partners.append(pp1)
pp3.partners.append(pp1)

pp1.photo = "./data/family_3-f5/antonio-p0/1.jpg"

pp4.father=pp1
pp4.mother=pp2

fam3_list=[pp1,pp2,pp3,pp4]
for p in fam3_list:
    add_new_member(fam3,p)


fam3.printFamily()
    
print(fam3.toJSON())
fam3=load_family("Family 3", fam3.family_id)
fam3.printFamily()
# fam3.showGraph()


#delete_member(fam3, pp2)
#delete_family_folder(fam3)

#family=load_family("family")
#family.printFamily()
# family.showGraph()
