# -*- coding: utf-8 -*-
import enolib
from enolib.locales import en
import enotype
import pandas as pd
import os
import re

PATH_BASIC = "../data/1-parsing/db-tracker/patterns/basic-info.csv"
PATH_DOMAIN = "../data/1-parsing/db-tracker/patterns/domain-info.csv"
PATH_FILTERS = "../data/1-parsing/db-tracker/patterns/pattern-info.csv"

PATH_TO_PATTERNS = "../data/helper/tracker-snippets/db/patterns/"
files_to_parse = os.listdir(PATH_TO_PATTERNS)

df_init_basic = pd.DataFrame({"name": [], "category": [],"url": [],"organization": [],"ghostry_id": [],"from_file": []})
df_init_basic.to_csv(PATH_BASIC, mode = "w")

df_init_domain = pd.DataFrame({"id": [], "domain": [],"from_file": []})
df_init_domain.to_csv(PATH_DOMAIN, mode = "w")

df_init_pattern = pd.DataFrame({"id": [], "filter": [],"from_file": []})
df_init_pattern.to_csv(PATH_FILTERS, mode = "w")

def read_pattern(filename):
    with open(filename, 'r') as file:
        input = file.read()

    document = enolib.parse(input, locale=en, source=filename)
    doc_basic ={}
    doc_basic["name"] = document.field('name').required_string_value()
    doc_basic["category"] = document.field("category").required_string_value()
    doc_basic["url"] = document.field("website_url").optional_string_value()
    doc_basic["organization"] = document.field("organization").optional_string_value()
    id_check = document.field("ghostery_id").optional_string_value()
    doc_basic["ghostery_id"] = id_check if id_check else 9999999
    doc_basic["from_file"]= re.search("[^/]*\\.eno", filename).group(0)
    
    domains = [item.optional_string_value() for item in document.fields("domains")]
    if len(domains) == 1:
        domains = domains[0].split("\n")
    filters = [item.optional_string_value() for item in document.fields("filters")]
    # print(len(filters))
    if len(filters) == 1:
        # print((filters[0]))
        if filters[0] != None:
            filters = filters[0].split("\n")

    doc_domains = {}
    doc_domains["id"] = doc_basic["ghostery_id"]
    doc_domains["domain"] = domains
    doc_domains["from_file"] = doc_basic["from_file"]
    doc_filters = {}
    doc_filters["id"] = doc_basic["ghostery_id"]
    doc_filters["filters"] = filters
    doc_filters["from_file"] = doc_basic["from_file"]

    # Throws an error when there are unhandled, mis-typed or unneeded fields in the document
    # document.assert_all_touched()
    df_basic = pd.DataFrame(doc_basic, index = ([1]))
    df_basic.to_csv(PATH_BASIC, mode="a", header=False)

    df_domains = pd.DataFrame(doc_domains)
    df_domains.assign(id = doc_basic["ghostery_id"])
    df_domains.to_csv(PATH_DOMAIN, mode="a", header=False)

    df_filters = pd.DataFrame(doc_filters)
    df_filters.assign(id = doc_basic["ghostery_id"])
    df_filters.to_csv(PATH_FILTERS, mode="a", header=False)

for i in files_to_parse:
    read_pattern(PATH_TO_PATTERNS +i)
# read_pattern("../data/helper/tracker-snippets/db/patterns/doubleclick.eno")
