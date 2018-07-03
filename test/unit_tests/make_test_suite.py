from FRUIT import *

test_modules = ["test_datetime.F90"]
driver = "test_driver.F90"
build_command = "make test_driver"

suite = test_suite(test_modules)
suite.write(driver)
