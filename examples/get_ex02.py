#simple Python script for downloading the MODFLOW-USG example problem
#from a public USGS ftp site.

from __future__ import print_function
import os
# import urllib2
try:
    # For Python 3.0 and later
    from urllib.request import urlretrieve
except ImportError:
    # Fall back to Python 2's urllib
    from urllib import urlretrieve
    
from zipfile import ZipFile

url = "http://water.usgs.gov/ogw/mfusg/02_quadtree.zip"

print('Attemping to download the file...')
file_name = url.split('/')[-1]
try:
    f, header = urlretrieve(url, file_name)
except:
    print ('Error.  Cannot download the file.')
    raise Exception()

#Unzip the file, and delete zip file if successful.
z = ZipFile(file_name)
try:
    z.extractall('./')
except:
    print('Could not unzip the file.  Stopping.')
    raise Exception()
z.close()
print('Deleting the zipfile...')
os.remove(file_name)
print('Done...')

