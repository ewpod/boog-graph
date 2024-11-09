#!/usr/bin/env python3

import argparse
import csv
import json


def process_maws(maws_csv):
    maws = {}
    with open(maws_csv) as maws_input:
        maws_reader = csv.DictReader(maws_input)
        for row in maws_reader:
            if row['Player'] not in maws:
                maws[row['Player']] = []
            player = maws[row['Player']]
            cleaned_row = {
                'Season': int(row['Season']),
                'Age': int(row['Age']),
                'MAWS': float(row['MAWS']),
                'Cumulative': float(row['Cumulative']),
            }
            player.append(cleaned_row)
    return maws


def options():
    parser = argparse.ArgumentParser()
    parser.add_argument('maws', help="MAWS input in CSV")
    parser.add_argument('--output', default='maws.json')
    args = parser.parse_args()
    return args


def main():
    args = options()
    maws = process_maws(args.maws)
    with open(args.output, 'w') as out:
        json.dump(maws, out)


if __name__ == '__main__':
    main()
