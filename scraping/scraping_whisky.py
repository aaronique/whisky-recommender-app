import os
import re
import base64
import math
import numpy as np
import pandas as pd
import cv2 as cv
import progressbar
import urllib

from tqdm import tqdm
from matplotlib import pyplot as plt
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException, NoSuchElementException


def wait_for_page(driver, timeout, xpath):
  try:
    WebDriverWait(driver, timeout).until(
        EC.presence_of_element_located((By.XPATH, xpath)))
    # print(driver.current_url + ' is ready!')
    return True
  except TimeoutException:
    # print(driver.current_url + ' failed to load!')
    return False


def get_profile(img, x):
  y = np.mean(np.nonzero(img[:, x, 1]))
  p = 100 - (y - 5) / 1.8
  p = math.ceil(p)
  if p < 0:
    p = 0
  if p > 100:
    p = 100
  return int(p)


def write_html(file, driver):
  with open(file, "w") as f:
    f.write(driver.page_source.encode('utf-8'))


def get_element(driver, xpath):
  try:
    return driver.find_element_by_xpath(xpath)
  except NoSuchElementException:
    return False


driver = webdriver.Chrome()

DISTILLER_DATA = os.path.abspath('data/spirits.csv')
WHISKY_DATA = os.path.abspath('data/whisky.csv')

SPIRIT_IMG_DIR = os.path.abspath('data/spirit_img')
WHISKY_IMG_DIR = os.path.abspath('data/whisky_img')

# spirit

page_col_names = ['id', 'name', 'link']

if os.path.exists(DISTILLER_DATA):
  df_page = pd.read_csv(DISTILLER_DATA)
  spirit_num = len(df_page)
else:
  driver.get("https://www.example.com/spirits")
  df_page = pd.DataFrame(columns=page_col_names)
  page_ready = wait_for_page(driver, 5, "//li[@class='undefined-content']")

  if page_ready:
    li = driver.find_elements_by_xpath("//li[@class='undefined-content']")
    a = driver.find_elements_by_xpath("//li[@class='undefined-content']//a")
    spirit_num = len(li)
    for i in tqdm(range(spirit_num)):
      df_page = df_page.append(pd.DataFrame([[i, li[i].text, a[i].get_attribute(
          "href")]], columns=page_col_names), ignore_index=True)

  df_page.to_csv(DISTILLER_DATA, index=False)

if not os.path.exists(SPIRIT_IMG_DIR):
  os.makedirs(SPIRIT_IMG_DIR)
if not os.path.exists(WHISKY_IMG_DIR):
  os.makedirs(WHISKY_IMG_DIR)

spirit_col_names = ['id', 'spirit.type', 'distiller.location', 'cost', 'expert.rating',
                    'customer.rating', 'age', 'abv', 'cask_type', 'tasting.note', 'author']
df_spirit = pd.DataFrame(columns=spirit_col_names)

for i in tqdm(range(2648, spirit_num)):
  driver.get(df_page[df_page['id'] == i]['link'][i])
  # page_ready = wait_for_page(driver, 5, '//canvas[@id="flavor-profile-chart"]') # whisky image
  page_ready = wait_for_page(
      driver, 5, '//*[@id="app"]/div/div[10]/div[1]/div[1]')  # spirit image

  if page_ready:
    spirit_type = get_element(
        driver, '//*[@id="app"]/div/div[10]/div[1]/div[3]/div[1]/h2[1]')
    spirit_type = spirit_type.text if spirit_type else np.nan

    distiller_location = get_element(
        driver, '//*[@id="app"]/div/div[10]/div[1]/div[3]/div[1]/h2[2]')
    distiller_location = distiller_location.text if distiller_location else np.nan

    cost = get_element(
        driver, '//*[@id="overview-tab"]/div/div[1]/ul/li[1]/div/div')
    cost = re.search('\d+', cost.get_attribute('class')
                     ).group() if cost else np.nan

    expert_rating = get_element(
        driver, '//*[@id="overview-tab"]/div/div[1]/ul/li[2]/div/span')
    expert_rating = expert_rating.text if expert_rating else np.nan

    customer_rating = get_element(
        driver, '//*[@id="overview-tab"]/div/div[1]/ul/li[3]/div/div/div[1]/span')
    customer_rating = customer_rating.text if customer_rating else np.nan

    age = get_element(
        driver, '//*[@id="overview-tab"]/div/div[2]/div[2]/ul/div/li[1]/div[2]')
    age = age.text if age else np.nan
    age = age if age != 'NAS' else np.nan

    abv = get_element(
        driver, '//*[@id="overview-tab"]/div/div[2]/div[2]/ul/div/li[2]/div[2]')
    abv = abv.text if abv else np.nan

    cask_type = get_element(
        driver, '//*[@id="overview-tab"]/div/div[2]/div[2]/ul/li[2]/div[2]')
    cask_type = cask_type.text if cask_type else np.nan

    tasting_note = get_element(
        driver, '//*[@id="overview-tab"]/div/div[3]/div[2]/p')
    tasting_note = re.sub(
        r'\s*"\s*', '', tasting_note.text) if tasting_note else np.nan

    author = get_element(
        driver, '//*[@id="overview-tab"]/div/div[3]/div[2]/div[1]/a')
    author = author.text if author else np.nan

    df_spirit = df_spirit.append(pd.DataFrame([[i, spirit_type, distiller_location, cost, expert_rating, customer_rating, age,
                                                abv, cask_type, tasting_note, author]], columns=spirit_col_names), ignore_index=True)
    df_spirit['id'] = df_spirit['id'].astype('int64')

    img_url = get_element(driver, '//*[@id="app"]/div/div[10]/div[1]/div[1]')
    if img_url:
      img_url = re.search(
          r'".+"', img_url.get_attribute('style')).group().replace("\"", "")
      _ = urllib.request.urlretrieve(
          img_url, os.path.join(SPIRIT_IMG_DIR, repr(i) + '.jpg'))

    base64_image = driver.execute_script(
        "return document.querySelector('.flavor-profile canvas').toDataURL('image/png').substring(21);")
    output_image = base64.b64decode(base64_image)
    with open(os.path.join(WHISKY_IMG_DIR, repr(i) + '.png'), 'wb') as f:
      _ = f.write(output_image)

driver.close()

# whisky

whisky_col_names = ['id', 'smoky', 'peaty', 'spicy', 'herbal', 'oily', 'full',
                     'rich', 'sweet', 'briny', 'salty', 'vanilla', 'tart', 'fruity', 'floral']
df_whisky = pd.DataFrame(columns=whisky_col_names)

for f in tqdm(os.listdir(WHISKY_IMG_DIR)):
  img = cv.imread(os.path.join(WHISKY_IMG_DIR, f), 1)[0:190, :]
  kernel = np.ones((2, 2), np.uint8)
  img = cv.erode(img, kernel, iterations=2)
  color = img[np.nonzero(img)[0], np.nonzero(img)[1]]
  if 88 < np.mean(color[:, 1]) < 90:
    left = np.min(np.nonzero(img)[1])
    right = np.max(np.nonzero(img)[1])
    col = np.linspace(left, right, num=14, dtype=int)
    df_whisky = df_whisky.append(pd.DataFrame([[int(f.replace(
        '.png', ''))] + [get_profile(img, c) for c in col]], columns=whisky_col_names))

df_whisky = df_whisky.sort_values(by=['id'], ascending=True)
df_whisky['id'] = df_whisky['id'].astype('int64')

df = pd.merge(df_whisky, df_page, on='id')
df = pd.merge(df, df_spirit, on='id')
df = df[page_col_names + spirit_col_names[1:] + whisky_col_names[1:]]

df.to_csv(WHISKY_DATA, index=False)
