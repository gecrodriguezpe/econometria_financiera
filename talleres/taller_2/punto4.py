# -*- coding: utf-8 -*-
"""
Media y desviación estándar 
"""

import numpy as np

lst = [-0.14, -0.15, 0.46, -0.02, 0.09]


def mean(lst): 
    num = 0
    # innerloop para calcular el valor esperado
    for elem in lst:
        num += elem
    return(num/len(lst))

def sd(lst):
    # valor esperado interior
    media = mean(lst)
    # innerloop para calcular el valor esperado exterior
    total = 0
    for elem in lst: 
        total += (elem - media)**2
    return(np.sqrt(total/(len(lst)-1)))


        
