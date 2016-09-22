#wzAAhelper

##Functions to make RSiteCatalyst API usage less painful

###How to install

This package has select dependencies on wzMisc, which is a public package (by me) hosted on my personal
github account. You will need to first install the `devtools` package, if you do not have it:

```
#If you do not have it already, devtools installation:    
install.packages("devtools")
```

Once this is complete, install the dependent package in any R session:  

```
devtools::install_github("slin30/wzMisc")
```

There is almost certainly a mechanism to directly install R packages from a private gitlab repo. I have not 
had time to look into this yet, so for now, please use the manual approach:

1. Navigate to the master branch (https://git.cm-elsevier.com/zhang1/wzaahelper/tree/master)
2. Download the tar.gz (even if you are running Windows)
3. Open an R session; probably most convenient to open the session in the file download location.  
    
Use the following code chunk:  

```
install.packages(path_to_file, repos = NULL, type="source")
#For example:
#don't run
# install.packages("C:/Users/zhang2/Downloads/wzaahelper-c9f847b34244629f4e8af95173c0c939d0f287a6.tar.gz", 
#                  repos = NULL, 
#                  type="source")
```

-----

    
