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


def get_element(driver, xpath):
  try:
    return driver.find_element_by_xpath(xpath)
  except NoSuchElementException:
    return False


driver = webdriver.Chrome()

DISTILLERY_DATA = os.path.abspath('app/distillery.csv')
DISTILLERY_DETAIL_DATA = os.path.abspath('app/distillery_detail.csv')

df_pre = pd.read_csv(DISTILLERY_DATA)
distillery_num = len(df_pre)

col_names = ['ID', 'Established', 'Status', 'Type', 'Country', 'Region',
             'Owner', 'Number of stills', 'Visitor center', 'Address', 'Phone', 'Website', 'Flavor', 'About']
df_distillery = pd.DataFrame(columns=col_names)

for i in tqdm(range(distillery_num)):
  # for i in tqdm(range(6)):
  driver.get(df_pre['link'][i])
  page_ready = wait_for_page(
      driver, 5, '//*[@id="info"]/div[2]/div[1]/div/a/h3')

  if page_ready:
    content = driver.find_element_by_id('details-list').text.split('\n')
    about = driver.find_element_by_class_name(
        'morecontent').text.replace('\n', '<br>')

    col = [''] * 14
    col[0] = df_pre['id'][i]

    for j in range(1, 12):
      if col_names[j] in content:
        try:
          col[j] = content[content.index(col_names[j]) + 1]
        except IndexError:
          pass

    try:
      flavor_ele = driver.find_element_by_class_name(
          'f-spiral').find_elements_by_tag_name('div')
      for f in flavor_ele:
        w = re.findall(r'\d+', f.get_attribute("style").split('; ')[-2])[0]
        n = f.text.lower()
        col[12] = col[12] + n + ',' + w + ','
    except NoSuchElementException:
      pass

    col[13] = about

    df_distillery = df_distillery.append(pd.DataFrame(
        [col], columns=col_names), ignore_index=True)

driver.close()

df_distillery.to_csv(DISTILLERY_DETAIL_DATA, index=False)
