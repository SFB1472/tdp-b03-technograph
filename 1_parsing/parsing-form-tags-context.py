# -*- coding: utf-8 -*-

import bs4
from bs4 import BeautifulSoup
import pandas as pd
import re
import json
import yaml
from sqlalchemy import create_engine

CURRENT_SPHERE = "German"

PATH_TO_FILES = "../data/0-preprocessing/" + CURRENT_SPHERE + "/"
FILES_TO_ANALYSE = "../data/2-analysing/"+ CURRENT_SPHERE + "/sites-with-comment-traces-form-tags.csv"
PARSED_DOC = []

with open("../config/config-secret.yaml", "r") as yamlfile:
    yaml_data = yaml.load(yamlfile, Loader=yaml.FullLoader)

def extract_info(elem):
    info = {}
    # print(str(elem.parents))
    temp_attrs = elem.attrs
    attr = []
    value = []
    for element in temp_attrs:
        list_counter = 0
        if(isinstance(temp_attrs[element], list)):
            for sub_element in temp_attrs[element]:
                list_counter +=1
                attr.append(element + str(list_counter))
                value.append(sub_element)
        else:
            attr.append(element)
            value.append(temp_attrs[element])
    parent_info = []
    parent_value = ""
    for parent in elem.parents:
        parent_path = {}
        parent_path["parent_name"] = parent.name
        parent_path["parent_id"] = parent.get("id")
        parent_path["parent_class"] = parent.get("class")
        parent_info.append(parent_path)
        parent_value = parent_value + str(parent.name) + ";" + str(parent.get("id")) + ";" + str(parent.get("class")) + ";"

    info = {"site": "0", "group": 0, "name" : elem.name, "attr": attr, "text" : elem.text, "value": value, "parent_path_str": parent_value, "parent_path_json": json.dumps(parent_info)}
    PARSED_DOC.append(info)

def iterarte_tags (all_form_tags):
    for elem in all_form_tags:

        if(type(elem) != bs4.element.NavigableString and type(elem) != bs4.element.Comment and type(elem) != bs4.element.Script):
        # if(len(list(elem.children))> 0):
            if(len(elem.contents)> 0):
                extract_info(elem)
                # print("es gibt noch kinder")
                iterarte_tags(elem)
            else:
                # print("keine kinder mehr")
                extract_info(elem)


def iterate_files():
   
    eng = create_engine("postgresql://"+ yaml_data["db"]["dsn_uid"] + ":"+ yaml_data["db"]["dsn_pwd"]+ "@"+ yaml_data["db"]["dsn_hostname"] + "/sfb1472_b03")
    files_to_analyse = pd.read_csv(FILES_TO_ANALYSE)

    for index, row in files_to_analyse.iterrows():
        html_file = row["sha1"] + ".html"
    # html_file = "0131ad7cec51b2fa0907587bd542af0d77147faf.html"
        print(html_file)
        html_file = PATH_TO_FILES + html_file
        with open(html_file) as fp:
            # html_file_clean = str(html_file).replace(PATH_TO_FILES, "").removesuffix(".html")
            contents = fp.read()
            # try: 
            contents = re.sub("\n", '', contents)
            page = BeautifulSoup(contents, "lxml")
            page.smooth()
            page.prettify()
            all_form_tags= page.find_all("form")

            iterarte_tags(all_form_tags)
            counted_items = 0
            for item in PARSED_DOC:
                # print(item)
                counted_items += 1
                item["group"] = counted_items
                item["site"] = row["sha1"]
                item["context_of"] = "form"
                item["sphere"] = CURRENT_SPHERE
                df = pd.DataFrame(item)
                # print(df)
                with eng.connect() as conn:
                    df.to_sql("tags_context", con=conn, if_exists='append', index=False)
                
                # except(RecursionError):
                #     print(row["sha1"])

iterate_files()