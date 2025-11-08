# Project 2 — Business Forecasting  
## Identifying the Most Important Metal in the Metals Index (1959–2010)  

**Author:** Giacomo Bizzotto  
**Course:** Business Forecasting  
**Date:** November 2025  

---

### 📘 Overview  
This project analyzes annual **real (CPI-adjusted)** metal prices from **1959–2010** to identify which metal most strongly influences the *Metals Index*.  
Two modeling approaches were applied using standardized (scaled) data:  

- **TSLM (Time Series Linear Model)** — measures linear relationships through standardized coefficients.  
- **Boruta Random Forest** — confirms nonlinear feature importance using ensemble learning.  

---

### 📊 Results  
- **Tungsten** is the most influential metal in the Metals Index.  
- **TSLM coefficient:** ≈0.56  
- **Boruta importance score:** ≈21  
- **Copper** (≈0.33) and **Chromium** (≈0.18) show moderate influence, while Nickel and Tin have minimal impact.  
- Both models agree, reinforcing Tungsten’s dominant role between 1959 and 2010.
