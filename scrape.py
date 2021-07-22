import requests
import pickle
import re
import os
import json




from bs4 import BeautifulSoup


TAG_RE = re.compile(r'<[^>]+>')

def directory_name(text):
    name = TAG_RE.sub('', str(text))
    name = name.split('-')[0]
    for ch in [',', '\'', ' ']:
        name = name.replace(ch, '_')
    name = name.replace('LTMN_', '')
    name = name.replace('&amp;', 'and')
    name = name.rstrip('_')

    return name

def file_name(text):
    name = TAG_RE.sub('', str(text))
    for ch in [',', '\'', ' ']:
        name = name.replace(ch, '_')
    name = name.replace('Long-term_monitoring_network_', '')
    name = name.replace('Long_term_monitoring_network_', '')
    name = name.replace('&amp;', 'and')

    return name


domain = 'http://publications.naturalengland.org.uk'
list_of_sites = domain + '/category/5316639066161152'

# Extracts the response as html
html_doc = requests.get(list_of_sites).text
# Create a BeautifulSoup object from the HTML
soup = BeautifulSoup(html_doc)

file_ad_list = []
data_dir = './Data/'
try:
    os.mkdir(data_dir)
    os.mkdir(data_dir + 'Vegetation/')
    os.mkdir(data_dir + 'Butterfly/')
except:
    pass

# Find all 'a' tags (which define hyperlinks)
a_tags = soup.find_all('a')
for link in a_tags:
    site = str(link.get('href'))
    if site.startswith('/publication'):
        site_url = domain + site
        site_html = requests.get(site_url).text
        site_soup = BeautifulSoup(site_html)

        dir_name = data_dir + directory_name(str(site_soup.title))
        #os.mkdir(dir_name)
        print('\n', dir_name, '\n')

        a_tags_site = site_soup.find_all('a')
        for site_link in a_tags_site:
            file_search = str(site_link.get('href'))
            if file_search.startswith('/file'):
                file_link = domain + file_search
                my_file = requests.get(file_link)

                if 'Metadata' in file_name(site_link):
                    pass
                elif 'vegetation' in file_name(site_link):
                    f_name = './Data/Vegetation/' + file_name(site_link) + '.xlsx'
                    print(file_name(site_link))
                    open(f_name, 'wb').write(my_file.content)
                    file_ad_list.append(f_name)
                elif 'butterfly' in file_name(site_link):
                    f_name = './Data/Butterfly/' + file_name(site_link) + '.xlsx'
                    print(file_name(site_link))
                    open(f_name, 'wb').write(my_file.content)
                    file_ad_list.append(f_name)
                else:
                    print(file_name(site_link) + ' - not saved')

with open(data_dir + "file_list.json", 'w') as f:
    json.dump(file_ad_list, f)



