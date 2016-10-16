# wzAAhelper

## Functions to make RSiteCatalyst API usage less painful

Version 0.0.0.9100

### How to install

This package has select dependencies on [wzMisc](https://github.com/slin30/wzMisc), which is a public package built by me 
(@zhang1) hosted on my personal github account. 

You will need to first install the `devtools` package, if you do not have it:

```{r}
# If you do not have it already, devtools installation:    
install.packages("devtools")
```

Once this is complete, install the dependent package in any R session:  

```{r}
devtools::install_github("slin30/wzMisc")
```

There is almost certainly a mechanism to directly install R packages from a private gitlab repo. I have not 
had time to look into this yet, so for now, please use the manual approach:

1. Navigate to the master branch (https://git.cm-elsevier.com/zhang1/wzaahelper/tree/master)
2. Download the tar.gz (even if you are running Windows)
3. Open an R session; probably most convenient to open the session in the file download location.  
    
Modify the following code chunk:  

```{r}
# install.packages(path_to_file, repos = NULL, type="source")
#For example:
# dontrun{
# install.packages("C:/Users/zhang2/Downloads/wzaahelper-c9f847b34244629f4e8af95173c0c939d0f287a6.tar.gz", 
#                  repos = NULL, 
#                  type="source")
# }
```

-----


### CI setup notes and caveats

Gitlab integrates with Docker for CI (continuous integration), to streamline package installation testing, among many other 
tasks. At the moment, this is not quite straightforward. This section is mainly to capture setup notes and will be updated as 
I figure out how to streamline the CI workflow.  

Any CI setup requires two things at minimum:  

1. An environment that runs the CI system. Typically, this is some kind of cloud server/service provider, but can be 
(and currently is) a local service.  
2. One or more configuration files, which tells the CI service what commmands to execute. This takes the form of a 
.yml file for gitlab, specifically a .gitlab-ci.yml. 
    - See the [current CI configuration](https://git.cm-elsevier.com/zhang1/wzaahelper/blob/master/.gitlab-ci.yml) for 
      this repository.  


To enable communication between #1 and #2, there are additional dependencies. For gitlab, this is a *runner*. Your 
best bet is to start with the [documentation](https://gitlab.com/gitlab-org/gitlab-ci-multi-runner), but be warned that 
even with the very thorough instructions, this is a non-trivial task, and I strongly suggest using a Unix environment. In 
fact, I have not yet figured out how to make this work in Windows.  

**NOTE: I have also not verified that this will work smoothly on Corporate-owned equipment, even with local admin**

If you are on Windows 7, here is the official general process:  

1. Navigate to https://www.docker.com/products/docker-toolbox
2. Click on the Windows link
3. Download and install, granting access privileges to Oracle applications if/when prompted.  


This installs, among other things, a VirtualBox application on your local system. This is really what you are looking for, and 
you can probably bypass the actual docker toolbox install altogether, although you may wish to verify that whatever version 
of VirtualBox you self-install is at least the same version as used by the docker toolbox.  
  
  
Next, you will want to:  

- Download a Linux distro image, e.g. [Ubuntu](https://www.ubuntu.com/download/desktop)
- Install a Linux VM via VirtualBox
- Follow the gitlab-ci-multi-runner [Linux instructions](https://gitlab.com/gitlab-org/gitlab-ci-multi-runner/blob/master/docs/install/linux-repository.md)  
    - In your Linux VM, via the terminal, of course.  
- Find the Docker image you want to use, and pull it in via e.g. 
    - `sudo docker pull rocker/r-base` 
        - If you wanted to use the r-base image for CI, by rocker
- Make sure the gitlab-ci-multi-runner service is running, via 
    - `sudo gitlab-ci-multi-runner status`
    - If service is not running, start it with `sudo gitlab-ci-multi-runner start`
- Create/edit your gitlab.yml file if necessary. 
- Commit the file, and this should trigger a test if all is working.  


From here, it is a matter of testing the various configuration commands in your .yml file, which can/should include other 
scripts to better handle dependencies, such as shell scripts. 

Once complete, and passed, you may wish to push your docker configuration to dockerhub for re-use and efficiency. Using the 
correct image can make a significant difference. For example, with r-base, additional `apt-get` args are required in either 
the yml or in a shell script sourced by the yml, in addition to installation of a few rather heavy packages. In total, this 
amounts to >9 minutes of test time. With `rocker/hadleyverse`, which comes pre-loaded with the required packages, the 
system dependencies and package installs are no longer required, and this cuts test time to <1 minute. 


#### Once you successfully set up CI

1. Each push to your repo will trigger an automated test and status message.  
2. If your Docker instance is local, as mine currently is, your runner will depend on the local system + VirtualBox Unix 
instance being active, and the gitlab-ci-multi-runner service running. Otherwise, your test will remain in 'pending' status 
unitl the CI service is back up and accessible.  

