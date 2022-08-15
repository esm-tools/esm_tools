import getpass
import os

import bcrypt
import yaml
from loguru import logger

HOST: str = "bhv-mongodb.awi.de"
"""str: the database hostname"""
USER: str = "myUserAdmin"
"""str: the admin user"""

def gen_auth():
# Get Password & Encrypt
    master_secret_key = getpass.getpass()
    salt = bcrypt.gensalt()
    pw = bcrypt.hashpw(master_secret_key.encode(), salt)

    os.makedirs(f"{os.getenv('HOME')}/.config/esm_tools/esm_mongo/", exist_ok=True)
## Username & Password needs to be stored somewhere
    with open(f"{os.getenv('HOME')}/.config/esm_tools/esm_mongo/auth_config.yaml", "w") as f:
        auth_info = {"password": pw, "username": getpass.getuser()}
        yaml.safe_dump(auth_info, f)



def check_auth():
    with open(f"{os.getenv('HOME')}/.config/esm_tools/esm_mongo/auth_config.yaml", "r") as f:
        auth_info = yaml.safe_load(f.read())
    pw = auth_info["password"]
    password_prompt = getpass.getpass()
    if bcrypt.checkpw(password_prompt.encode(), pw):
        logger.success("Authentication matches salted password!")
        return password_prompt
    logger.error("Your user inputted password does not match!")
