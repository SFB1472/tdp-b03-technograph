# -*- coding: utf-8 -*-

import bs4
from bs4 import BeautifulSoup
import pandas as pd
from os import walk
import pathlib

CURRENT_SPHERE = "Dutch"

PATH_TO_FILES = "../data/0-preprocessing/" + CURRENT_SPHERE + "/"
PATH_TO_SAVE_AT = "../data/1-parsing/doctype/" + CURRENT_SPHERE + "/"
NAME_TO_SAVE = "doctype.csv"



def iterate_files():

    df_init = pd.DataFrame({"site": [], "doctype": []})
    df_init.to_csv(PATH_TO_SAVE_AT + "/" + NAME_TO_SAVE, mode = "w")

    for html_file in pathlib.Path(PATH_TO_FILES).rglob('*.html'):
        
        print(html_file)
        with open(html_file) as fp:
            doctype = []
            site = []
            html_file_clean = str(html_file).replace(PATH_TO_FILES, "").removesuffix(".html")
            site.append(html_file_clean)
            contents = fp.read()
            try: 
                page = BeautifulSoup(contents, "lxml")
                page.prettify()
                snippet = extract_doctype(page)
                doctype.append(snippet)
            except(RecursionError):
                doctype.append("NA")
                print(html_file_clean)
            doctype_data = {"site": site, "doctype": doctype}
            df_doctype_data = pd.DataFrame(data = doctype_data)
            df_doctype_data.to_csv(PATH_TO_SAVE_AT + "/" + NAME_TO_SAVE, mode="a", header=False)
            # print(snippet)

def extract_doctype(soup):
    items = [item for item in soup.contents if isinstance(item, bs4.Doctype)]
    return items[0] if items else "NA"
    


iterate_files()