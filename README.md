# easy-deploy

A quick and easy way to implement blue-green deployment with Docker.

The app covers these main steps:
    -   Load code from Source control
    -   Build
    -   Run tests
    -   Switch running version seamlessly

## TODO

## Things to Juggle
    -   Docker Images
    -   Docker Containers
    -   Docker Running instances
    -   Ports being used
    -   Old app files
    -   Be able to run multiple easy-deployments in same machine
    ?   Listen to webhook

## Deployment Config

`easy-deploy` takes a `yaml` configuration file that specifies how the build will work.
We always assume that your app runs on port 8080

### Dockerhub deployment

-   Deploy specific tag
-   Deploy latest tag

``` yaml
DOCKER_IMAGE: lazamar/silver-magpie-haskell

RUN_ARGS: -v /home/myapp/_env:/home/app/_env

# This is just to check whether the server is working correctly. If
# it succeeds the swap goes forward
SMOKE_TEST: curl http://localhost:$PORT
```  

### Github deployment

-   Deploy specific commmit
-   Deploy latest commit

``` yaml
GITHUB_REPO: https://github.com/lazamar/silver-magpie-backend

RUN_ARGS: -v /home/myapp/_env:/home/app/_env

SMOKE_TEST: curl http://localhost:$PORT
```  


### Super customised deployment  
``` yaml
## config.yaml
# Available variables:
#   $PORT       : The port your app should serve to
#   $TEMP_DIR    : The temporary directory for your build files
#   $IMAGE_NAME  : Name of docker image to be built
#   

# This command must return a unique identifier for the build so that
# we don't build the same code more than once.
UUID: ??

LOAD: git clone https://github.com/lazamar/silver-magpie-backend $TEMP_DIR
# In build you can assume the current working directory is $TEMP_DIR
BUILD: docker build --tag $IMAGE_NAME .

RUN: docker run -d -p $PORT:8080 -v /home/myapp/_env:/home/app/_env

SMOKE_TEST: curl http://localhost:$PORT
```
