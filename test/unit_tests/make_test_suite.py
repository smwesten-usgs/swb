from FRUIT import *
import glob

test_modules = glob.glob("test_*.F90")

driver = "fruit_driver.F90"
build_command = "make fruit_driver"

suite = test_suite(test_modules)
suite.write(driver)
