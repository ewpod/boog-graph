#!/usr/bin/env python3

import argparse
import csv
import json


def process_boog(boog_csv):
    boog = {}
    with open(boog_csv) as boog_input:
        boog_reader = csv.DictReader(boog_input)
        for row in boog_reader:
            if row['name'] not in boog:
                boog[row['name']] = []
            player = boog[row['name']]
            #cleaned_row = {
            #    'Age': int(row['Age']),
            #    'BOOG': float(row['BOOG']),
            #    'Cumulative': float(row['Cumulative']),
            #}
            cleaned_row = [
                int(row['age']),
                float(row['season_BOOG']),
                float(row['career_to_date_BOOG']),
            ]
            player.append(cleaned_row)
    return boog


def options():
    parser = argparse.ArgumentParser()
    parser.add_argument('boog', help="BOOG input in CSV")
    parser.add_argument('--output', default='boog.json')
    args = parser.parse_args()
    return args


def main():
    args = options()
    boog = process_boog(args.boog)
    with open(args.output, 'w') as out:
        json.dump(boog, out, sort_keys=True, separators=(',', ':'))


if __name__ == '__main__':
    main()
