# -*- coding: utf-8 -*-

import bs4
from bs4 import BeautifulSoup
import numpy as np
import pandas as pd
import re
import json
# import yaml
# from sqlalchemy import create_engine

CURRENT_SPHERE = "German"

# PATH_TO_FILES = "../data/0-preprocessing/" + CURRENT_SPHERE + "/"
PATH_TO_FILES_1 = "../data/0-preprocessing/" + CURRENT_SPHERE + "/"
PATH_TO_FILES_2 = "../data/0-preprocessing/" + CURRENT_SPHERE + "-2/"
FILES_TO_ANALYSE = "../data/1-parsing/tags/"+ CURRENT_SPHERE + "/sites-and-findings-for-context-parsing.csv"

PATH_TO_SAVE_AT = "../data/1-parsing/tags-context/" + CURRENT_SPHERE + "/"
NAME_TO_SAVE = "extracted-pathes.csv"

def get_html_page(html_file):
    with open(html_file) as fp:                
        contents = fp.read()
        # try: 
        contents = re.sub("\n", '', contents)
        page = BeautifulSoup(contents, "lxml")
        # page.smooth()
        page.prettify()
    return page

def iterate_files():
   
    current_file = ""
    files_to_analyse = pd.read_csv(FILES_TO_ANALYSE)
    df_init = pd.DataFrame({"sha1" : [], "tag": [], "name" : [], "attr" : [], "depth" :[]})
    df_init.to_csv(PATH_TO_SAVE_AT + "/" + NAME_TO_SAVE, mode = "w")

    for index, row in files_to_analyse.iterrows():
        html_file = row["sha1"] + ".html"

    # html_file = "00012777602a9a99e667f0e4b5416440a9d0f081.html"
        print(html_file)
        
        if(html_file != current_file):    
            try:
                html_file_1 = PATH_TO_FILES_1 + html_file
                page = get_html_page(html_file_1)
            except FileNotFoundError:
                html_file_2 = PATH_TO_FILES_2 + html_file
                page = get_html_page(html_file_2)
   
            current_file = html_file

        path = {}
        path["sha1"] = row["sha1"]
        path["tag"] = row["tag"]
        path["name"] = row["name"]
        path["attr"] = row["attr"]
        special_tags = page.find_all(attrs={(row["name"]): str(row["attr"])})
        
        for item in special_tags:
            print("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
            parents = item.find_parents()
            parents = reversed(parents)
            parent_i = 0
            for tag in parents:
                parent_i += 1
                path["depth_" + str(parent_i)] = (tag.name + " " + str(tag.attrs))
            path["depth_" + str(parent_i+1)]  = item.name + " " + str(item.attrs)

        df_path = pd.DataFrame(data = [path])
        df_path.to_csv(PATH_TO_SAVE_AT + "/" + NAME_TO_SAVE, mode="a", header=False)

iterate_files()