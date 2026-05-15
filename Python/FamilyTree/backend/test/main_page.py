import streamlit as st

# Main page content
st.markdown("# Main page 🎈")
st.sidebar.markdown("# Main page 🎈")

def func():
    st.balloons()

button = st.sidebar.button("Touch for a surprise", on_click=func)

