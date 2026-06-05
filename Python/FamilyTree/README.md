# FamilyTree

FamilyTree is a small Python desktop application for creating, visualizing, and managing family trees.

The project provides a graphical interface where users can create families, add people, connect family relationships, and inspect personal and health-related information such as blood type and diseases.

## Features

* Create and load families
* Add new people to a family
* Load existing people into different families
* Support people belonging to more than one family
* Edit personal information
* Remove a person from the current family
* Remove a person from the global people database
* Connect family relations:

  * partners
  * father and child
  * mother and child
* Remove existing family connections
* Display a visual family tree
* Show detailed information for each person
* Store optional photo paths for people
* Display family blood type summaries
* Display family disease summaries
* Track diseases through relatives

## Project Structure

```text
.
├── backend.py          # Backend logic for creating, loading, updating, and removing data
├── classes.py          # Person and Family data classes
├── pyside_display.py   # PySide6 graphical user interface
└── data/               # Automatically created folder for saved people and families
```

## Data Model

The project uses two main classes:

### Person

A person contains:

* name
* birth date
* death date
* health information
* photo path
* identifier
* list of families they belong to

### Family

A family contains:

* name
* identifier
* list of members
* family relations between members

Family relations include:

* father
* mother
* siblings
* partners
* kids

## Requirements

This project uses Python and PySide6.

Install the required dependency with:

```bash
pip install PySide6
```

## How to Run

Run the graphical interface with:

```bash
python pyside_display.py
```

When the application starts, it automatically creates the required data folders if they do not already exist.

## Usage

From the initial screen, you can either create a new family or load an existing one.

After opening a family, you can:

1. Add people to the family.
2. Load existing people into the current family.
3. Select people in the tree to inspect their information.
4. Connect people as partners, parents, or children.
5. Remove existing relationships.
6. View family-level health reports.
7. Track diseases through relatives.

## Storage

The application stores data locally in JSON files inside the `data/` folder.

People are stored globally, so the same person can belong to more than one family. Families store their own member lists and relationship information.

## Notes

This project is currently a simple local desktop application. It is intended for experimenting with family-tree visualization, relationship management, and basic health-information tracking.

Future improvements may include:

* Better date handling
* More advanced medical history support
* Improved layout for large families
* Exporting family trees
* More robust validation of family relationships
* Packaging the application for easier installation

## License

This project is licensed under the MIT License.

You are free to use, copy, modify, distribute, sublicense, and sell copies of the software, provided that the original copyright notice and license text are included.

## Remark

The README was written by ChatGPT
