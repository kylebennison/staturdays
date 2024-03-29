# staturdays 0.1.0

Code for plotting, modeling and predicting college football data.

This repo uses the [collegefootballdata.com](www.collegefootballdata.com) API to pull in play by play data, betting data, and more advanced stats to analyze it and make predictions.

# Contributing
To contribute, [pick an issue or create a new issue](https://github.com/kylebennison/staturdays/issues) and open a new branch off of dev with the name feature/issue_number_and_name.

When your work is ready to be reviewed, open a Pull Request to merge your branch into dev and fill out the PR template.

You will need to [install the statRdaysCFB package](https://github.com/kylebennison/statRdaysCFB) in order to access data and run most scripts. You will also need a free CFBdata [API key](https://collegefootballdata.com/key).

# Getting Started

First, create a venv and install the requirements.

```bash
# Create and activate
python3 -m venv .venv

source .venv/bin/activate
```

<details>
    <summary>Windows</summary>

    source .venv/Scripts/Activate
</details>

```bash
# Upgrade pip
python -m pip install --upgrade pip

# Install requirements
python -m pip install -r requirements.txt
```

Next, you'll have to set your CFBData API key as an environment variable.

```bash
CFBD_API=<your_key>
```

<details>
    <summary>Windows</summary>

    set CFBD_API=<your_key>
</details>
