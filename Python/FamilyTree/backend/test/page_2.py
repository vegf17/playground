import streamlit as st

st.markdown("# Page 2 ❄️")
st.sidebar.markdown("# Page 2 ❄️")


#slider with minimal value 0 and maximal value 20
x = st.slider(
    'x',
    0.0, 20.0
)
st.write(x, 'squared is', x*x)

#slider with generic limits
xx = st.slider('xx')
st.write(xx, "squared is", xx*xx)

#checkbox, which makes appear/disappear contents
if st.checkbox('Show text input'):
    st.text_input("Your name", key="name")
    st.session_state.name
    y = st.text_input("Value", key="value")
    #st.session_state.value
    st.write(y, "squared is", float(y)*float(y))
    

#add a selectbox to the sidebar (left side)
add_selectbox = st.sidebar.selectbox(
    "Options",
    ("1","2","3")
)

#add a slider to the sidebar
add_slider = st.sidebar.slider(
    'Range',
    0.0, 100.0, (25.0, 75.0)
)

#define columns
left_column, right_column = st.columns(2)

#insert a button in the left column
left_column.button("Hello World!")

#insert a "with" block in the right column
with right_column:
    chosen = st.radio(
        'Choices',
        ("1","2","3","4")
    )
    st.write(f"Number chosen was {chosen}")


add_smt = st.sidebar.button("Add something")
rmv_smt = st.sidebar.button("Remove something")
show_smt = st.sidebar.button("Show something")

