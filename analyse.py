# read all the text files in results
# for each file, generate the average score for each category
# print the averages for each category for each file

import os
import sys
import json
import numpy as np

def main():
    # get the path to the results directory
    path = "results"

    # get the list of files in the results directory
    files = os.listdir(path)

    # for each file, get the average score for each category
    for file in files:
        # open the file
        with open(path + "/" + file, "r") as f:
            # read the file
            data = f.read()
            lines = data.split("\n")
            print(f"{file}\n{generate_average(lines)}\n")
        


def getCategories(data):
    categories = []
    for line in data:
        if line == "":
            continue
        # get the text before the :
        category = line.split(":")[0]
        if category not in categories:
            categories.append(category)
    return categories

def generate_average(data):
    categories = getCategories(data)
    # create a dictionary to store the counts
    counts = {}
    average = {}
    for category in categories:
        average[category] = 0
        counts[category] = 0
    total = 0
    for line in data:
        if line == "":
            continue
        total += 1
        for category in categories:
            if category in line:
                counts[category] += 1
    for category, count in counts.items():
        average[category] = count / total
    return average
    


main()