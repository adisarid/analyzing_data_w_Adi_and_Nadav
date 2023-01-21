import requests
from tqdm import tqdm
from bs4 import BeautifulSoup

RANGE = 5954

def retrieve_record(post_number):
    html = requests.get(f'https://irrelevant.org.il/?p={post_number}')
    if html.status_code == 404:
        return None
    soup = BeautifulSoup(html.text, 'html.parser')
    title = soup.select_one('.entry-title').get_text()
    explained, source = soup.select_one('.entry').get_text().split('המקור:')
    return dict(title=title, explained=explained, source=source)

# 1 liner if we dont care about the progress bar
# scraping_results = [rec for rec in map(retrieve_record, range(1, RANGE)) if rec is not None]

# with tqdm auto progress bar. needs the total specified because map() is a generator
scraping_results = []
for rec in tqdm(map(retrieve_record, range(1, RANGE)), total=RANGE):
    if rec is not None:
        scraping_results.append(rec)
