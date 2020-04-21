# Parse the webpage
from requests import get
from requests.exceptions import RequestException
from contextlib import closing
from bs4 import BeautifulSoup
from pymongo import MongoClient
from datetime import datetime
import os

# Change date in the format dd-mm-yyyy
#curDate = datetime.today().strftime('%d-%m-%Y')
#curDate = "21-05-2019"

transferCol = ""
def initialise_db():
    dbClient = MongoClient(port=27017)
    if not "footballtransfers" in dbClient.list_database_names():
        db = dbClient["footballtransfers"]
        
    else:
        db = dbClient.footballtransfers
    global transferCol
    transferCol = db["bets"]
    transferCol.create_index( [("player", 1), ("club", 1), ("betOutcome", 1)], unique= True )
    
def getURL(url):

    try:
        with closing(get(url, stream=True)) as resp:
            if is_correct_response(resp):
                parse_content(resp.content)
            else:
                return None

    except RequestException as e:
        log_error('Error during requests to {0} : {1}'.format(url, str(e)))
        return None

def get_doc(doc, fileDate):
    f = open(doc, "r")
    parse_content(f.read(), fileDate)

def is_correct_response(resp):
    """
    Returns True if the response seems to be HTML, False otherwise.
    """
    content_type = resp.headers['Content-Type'].lower()
    return (resp.status_code == 200 
            and content_type is not None 
            and content_type.find('html') > -1)


def log_error(e):

    print(e)

def parse_content(response, fileDate=None):
    initialise_db()
    html_cont = BeautifulSoup(response, 'html.parser')
    for main_div in html_cont.select('div'):
        if main_div.has_attr('class') and main_div['class'] == ['transfer_specials_event']:
            #print(main_div.text)
            player_det = main_div.find('span', attrs={'class': 'description'})
            player_name = player_det.text.strip().split(' to sign')[0]
            for clubBets in main_div.find_all('tr', attrs={'class': 'body'}):
                betDict = {}
                betDict["date"] = fileDate
                betDict["player"] = player_name
                betDict["betId"] = clubBets.find('span')['data-bet']
                betDict["club"] = clubBets.find('span')['data-outcome_description']
                betDict["betPrice"] = clubBets.find('span')['data-price']
                betDict["betOutcome"] = clubBets.find('span')['data-price_formatted']
                betDict["marketId"] = clubBets.find('span')['data-market_id']
                try:
                    transferCol.insert_one(betDict)
                except:
                    pass
                
            
BASE_DIR = "/Users/nikhitha/Documents/Seminar/BettingFilesData/"
files = []
files.extend(os.path.join(BASE_DIR, file) for file in sorted(os.listdir(BASE_DIR)) if file.endswith('txt'))           
for i in range(0, len(files)):
    file = files[i]
    fileDate = file.split('/')[-1].split('.')[0]
    get_doc(file, fileDate)
    

#get_doc('/Users/nikhitha/Documents/Seminar/May21.txt')