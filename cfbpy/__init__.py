import os
import logging

logging.basicConfig()
logger = logging.getLogger(name="preprocess")
logger.setLevel(logging.INFO)


def check_env():
    KEY = os.getenv("CFBD_API")

    if KEY is None:
        logger.warning("Key CFBD_API not found in env. Please set it!")
        res = input("Enter key: ")
        os.environ["CFBD_API"] = res


check_env()
