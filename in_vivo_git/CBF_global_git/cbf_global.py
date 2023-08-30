#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug 30 14:06:21 2023

@author: ali
"""

import nibabel as nib
import os, subprocess, sys, shutil, glob, re, fnmatch
import socket
from dipy.segment.mask import median_otsu
from dipy.io.image import load_nifti, save_nifti
import numpy as np
from scipy.cluster.vq import kmeans, vq
import pandas as pd




excel_path = '/Users/ali/Desktop/Aug23/CVN/rba/in_vivo/CBF_GLOBAL/master_sheet_cvn.xlsx'
excel = pd.read_excel(excel_path)
excel['cbf'] = 0



path_CBF = '/Users/ali/Desktop/Aug23/CVN/rba/in_vivo/CBF_GLOBAL/CBF_images/'
files = os.listdir(path_CBF)
files = [i for i in files if 'CBF_to_MDT' in i]
subjs = [i.partition('_CBF_to_MDT.nii.gz')[0] for i in files]


for subj in subjs:
    temp_path = path_CBF + subj + '_CBF_to_MDT.nii.gz'
    nii = nib.load(temp_path)
    data = nii.get_fdata()
    np.mean(data[data!=0])
    index = np.where(subj == excel['SAMBA Brunno'])
    
    if np.size(index, axis=None)>0:
        #print(subj)
