# -*- coding: utf-8 -*-
"""
Created on Sat Jul 11 17:25:54 2020

@author: Ranna
"""

import numpy as np
import pandas as pd

form = pd.read_csv('C:\\Users\\Ranna\\Documents\\excelR\\assignments\\Hypothesis testing\\Costomer+OrderForm.csv')
form.head()
# Defective as 0 and Error Free as 1

form.Phillippines[form.Phillippines == 'Error Free'] = 0
form.Phillippines[form.Phillippines == 'Defective'] = 1

form.Indonesia[form.Indonesia == 'Error Free'] = 0
form.Indonesia[form.Indonesia == 'Defective'] = 1

form.Malta[form.Malta == 'Error Free'] = 0
form.Malta[form.Malta == 'Defective'] = 1

form.India[form.India == 'Error Free'] = 0
form.India[form.India == 'Defective'] = 1

form1 = pd.DataFrame([form['Phillippines'].value_counts(),form.Indonesia.value_counts(),form['Malta'].value_counts(),form['India'].value_counts()])

Chisqres=scipy.stats.chi2_contingency(form1)
Chisqfin=[['','Test Statistic','p-value'],['Sample Data',Chisqres[0],Chisqres[1]]]
Chisqfin

# p-value = 0.2771 > 0.05, failed to reject null hypothesis
# H0 = there is no defective%
# Ha = there is a defective % which varies by centre
# Hence, there is no defective% which varies by centre







