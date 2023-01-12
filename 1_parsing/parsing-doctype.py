# -*- coding: utf-8 -*-
import csv
import re
import os.path
import time
import bs4
from bs4 import BeautifulSoup
import lxml
import pandas as pd
from os import walk
import glob
import pathlib

PATH_TO_FILES = "../data/0-preprocessing/German/"
PATH_TO_SAVE_AT = "../data/1-parsing/doctype/German/"
NAME_TO_SAVE = "doctype.csv"



def iterate_files():

    df_init = pd.DataFrame({"site": [], "doctye": []})
    df_init.to_csv(PATH_TO_SAVE_AT + "/" + NAME_TO_SAVE, mode = "w")

    for html_file in pathlib.Path(PATH_TO_FILES).rglob('*.html'):
        
        print(html_file)
        with open(html_file) as fp:
            contents = fp.read()
            page = BeautifulSoup(contents, "lxml")
            page.prettify()
            snippet = extract_doctype(page)
            site = []
            doctype = []
            site.append(html_file)
            doctype.append(snippet)
            doctype_data = {"site": site, "doctype": doctype}
            df_doctype_data = pd.DataFrame(data = doctype_data)
            df_doctype_data.to_csv(PATH_TO_SAVE_AT + "/" + NAME_TO_SAVE, mode="a", header=False)
            # print(snippet)

def extract_doctype(soup):
    items = [item for item in soup.contents if isinstance(item, bs4.Doctype)]
    return items[0] if items else "NA"
    


iterate_files()