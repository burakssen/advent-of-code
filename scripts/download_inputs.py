#!/usr/bin/env python3

import os
import argparse
import requests
import datetime
from typing import List, Optional, Union
import concurrent.futures

class AOCInputDownloader:
    def __init__(self, session_cookie: Optional[str] = None):
        """
        Initialize the Advent of Code input downloader.
        
        :param session_cookie: Optional AOC session cookie for authentication
        """
        if not session_cookie:
            session_cookie = os.environ.get('AOC_SESSION_COOKIE')
        
        if not session_cookie:
            try:
                with open(os.path.expanduser('~/.aoc_session_cookie'), 'r') as f:
                    session_cookie = f.read().strip()
            except FileNotFoundError:
                print("No session cookie found. Please provide one.")
                session_cookie = None

        self.session_cookie = session_cookie
        self.headers = {
            'Cookie': f'session={self.session_cookie}' if self.session_cookie else ''
        }

    def download_input(self, year: int, day: int, output_dir: str = '../inputs', force: bool = False) -> bool:
        """
        Download input file for a specific Advent of Code day.
        
        :param year: Year of the Advent of Code
        :param day: Day of the challenge
        :param output_dir: Directory to save input files
        :param force: Force re-download even if file exists
        :return: True if input is downloaded or already exists, False otherwise
        """
        # Validate inputs
        if not (1 <= day <= 25):
            print(f"Invalid day: {day}. Day must be between 1 and 25.")
            return False

        # Create output directories
        year_dir = os.path.join(output_dir, str(year))
        os.makedirs(year_dir, exist_ok=True)

        # Construct output file path
        output_path = os.path.join(year_dir, f"{day}.txt")

        # Skip if file exists and force flag is not set
        if os.path.exists(output_path) and not force:
            print(f"Input for Year {year}, Day {day} already exists. Skipping download.")
            return True

        # Check if input is available (not future dated)
        if datetime.datetime.now().date() < datetime.date(year, 12, day):
            print(f"Day {day} has not arrived yet.")
            return False

        try:
            # Construct URL for input
            url = f'https://adventofcode.com/{year}/day/{day}/input'

            # Send request to download input
            response = requests.get(url, headers=self.headers)
            
            if response.status_code == 400 or response.status_code == 401:
                print("Authentication failed. Check your session cookie.")
                return False
            elif response.status_code != 200:
                print(f"Error downloading input. Status code: {response.status_code}")
                return False

            # Write input to file
            with open(output_path, 'w') as f:
                f.write(response.text.strip())
            
            print(f"Successfully downloaded input for Year {year}, Day {day}")
            return True

        except requests.RequestException as e:
            print(f"Request error: {e}")
            return False

    def download_inputs(self, years: List[int], days: Optional[List[int]] = None, force: bool = False) -> None:
        """
        Download inputs for specified years and days in parallel.
        
        :param years: List of years to download
        :param days: Optional list of days to download (if None, download all days)
        :param force: Force re-download of existing files
        """
        # If no days specified, download all days
        if days is None:
            days = list(range(1, 26))

        # Use ThreadPoolExecutor for parallel download
        with concurrent.futures.ThreadPoolExecutor() as executor:
            futures = []
            for year in years:
                for day in days:
                    futures.append(executor.submit(self.download_input, year, day, force=force))

            # Wait for all downloads to complete
            for future in concurrent.futures.as_completed(futures):
                future.result()  # Result will throw exception if any occurred

def parse_range_arg(arg: Union[str, int]) -> List[int]:
    """
    Parse range arguments for years or days.
    
    :param arg: Argument string or integer (e.g., '2020', '2020-2022', '1-5', 2023)
    :return: List of integers
    """
    arg_str = str(arg)
    if '-' in arg_str:
        start, end = map(int, arg_str.split('-'))
        return list(range(start, end + 1))
    return [int(arg_str)]

def main():
    # Set up argument parser
    parser = argparse.ArgumentParser(description='Download Advent of Code input files')
    parser.add_argument('-y', '--years', type=parse_range_arg, default=[year for year in range(2015, datetime.datetime.now().year + 1)],
                        help='Year or year range (e.g., 2020 or 2020-2022)')
    parser.add_argument('-d', '--days', type=parse_range_arg, 
                        help='Day or day range (e.g., 5 or 1-10)')
    parser.add_argument('--cookie', type=str, 
                        help='AOC session cookie (overrides env/file)')
    parser.add_argument('--output', type=str, default='../inputs', 
                        help='Output directory for input files')
    parser.add_argument('--force', action='store_true',
                        help='Force re-download of existing files')

    # Parse arguments
    args = parser.parse_args()

    # Initialize downloader
    downloader = AOCInputDownloader(args.cookie)
    
    # Download inputs
    downloader.download_inputs(args.years, args.days, force=args.force)

if __name__ == '__main__':
    main()
