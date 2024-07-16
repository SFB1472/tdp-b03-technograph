# -*- coding: utf-8 -*-

import bs4
from bs4 import BeautifulSoup
import numpy as np
import pandas as pd
import re
from ast import literal_eval
# import yaml
# from sqlalchemy import create_engine

CURRENT_SPHERE = "German"

# PATH_TO_FILES = "../data/0-preprocessing/" + CURRENT_SPHERE + "/"
PATH_TO_FILES_1 = "../data/0-preprocessing/" + CURRENT_SPHERE + "/"
PATH_TO_FILES_2 = "../data/0-preprocessing/" + CURRENT_SPHERE + "-2/"
FILES_TO_ANALYSE = "../data/1-parsing/tags/"+ CURRENT_SPHERE + "/tag-context-parsing-2.csv"
TRACES_FOUND = "../data/2-analysing/" + CURRENT_SPHERE + "/found-traces.csv"
PATH_TO_SAVE_AT = "../data/1-parsing/tags-context/" + CURRENT_SPHERE + "/"
NAME_TO_SAVE = "context-all-traces-2.csv"

def get_attrs(item):
    info = {}
    # print(str(elem.parents))
    # temp_attrs = elem.attrs
    attr = []
    value = []
    text = []
    for element in item.attrs:
        list_counter = 0
        if(isinstance(item.attrs[element], list)):
            for sub_element in item.attrs[element]:
                # list_counter +=1
                attr.append(element)
                value.append(sub_element)
                if (type(element) == str):
                    text.append(element.text)
                else:
                    text.append("")
        else:
            # print(element)
            attr.append(element)
            value.append(item.attrs[element])
            if (type(element) == str):
                text.append(element.text)
            else:
                text.append("")
    info = {"attr" : attr, "value": value, "text": text}
    df_info = pd.DataFrame(info)
    df_info["tag"] = item.name
    return(df_info)

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
    found_traces = pd.read_csv(TRACES_FOUND)
    found_traces_list = found_traces["comment_trace"].to_list()
    # "sha1", "tag", "attr", "value", "group", "context_of", "sphere"
    df_init = pd.DataFrame({"sha1" : [], "tag": [], "attr" : [], "value" : [], "text" : [], "group" :[], "sphere" :[], "context_path" :[]})
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
            counted_items = 0

        attrs_obj = eval(row["attrs"])
        
        ## searching for the attribute with a trace to commenting, instead of randomly picking one
        search_for_attr = {}
        if len(attrs_obj) > 0:
            for attr in attrs_obj:
                for trace in found_traces_list:
                    if str(attrs_obj[attr]).find(trace) > -1:
                        search_for_attr[attr] = attrs_obj[attr]

        special_tags = page.find_all(attrs=search_for_attr)
        
        for item in special_tags:
            # print("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
            # print("parent: " + item.name)
            # print(item)
            counted_items += 1
            if(type(item)== bs4.Tag):
                attr_info = get_attrs(item)
                # print(item.text)
            
            for child in item.descendants:
                # print("xxxxxxx")
                # print(child.text)
                if(type(child)== bs4.Tag):
                    # print((child.attrs))
                    child_attr_info = get_attrs(child)
                    attr_info = pd.concat([attr_info, child_attr_info])
                    
            attr_info["sha1"] = row["sha1"]
            attr_info["group"] = counted_items
            attr_info["context_path"] = row["attrs"]
            attr_info["sphere"] = CURRENT_SPHERE
            # attr_info["text"] = text
            attr_info = attr_info[["sha1", "tag", "attr", "value", "text", "group", "sphere", "context_path"]]
            # print(attr_info)
            attr_info.to_csv(PATH_TO_SAVE_AT + "/" + NAME_TO_SAVE, mode="a", header=False)
        
iterate_files()