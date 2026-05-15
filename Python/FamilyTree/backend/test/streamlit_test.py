import streamlit as st

#define pages
main_page = st.Page("main_page.py", title="Main Page", icon="😎")
page_2 = st.Page("page_2.py", title="Page 2", icon="👀")

#set up navigation between pages
pg = st.navigation([main_page, page_2])

#run the selected page
pg.run() 
