# easy-deploy

A quick and easy way to implement [blue-green deployment](https://martinfowler.com/bliki/BlueGreenDeployment.html) with Docker.

This will allow you to do zero downtime deployments.

## How to use

It works very similarly to `docker run`, which will be called in the background.
Let's run my `lazamar:0.5` image

```
easy-deploy -p 8080:8080 lazamar:0.5
>
> Starting Blue image.
> Waiting for 3 seconds for server to start
> Switching proxy to Blue
> Blue is now the main container
```

We can see the image and proxy server are running

```
docker ps

> CONTAINER ID        IMAGE               ...         PORTS                            NAMES
> c0345092b90c        nginx:latest        ...         80/tcp, 0.0.0.0:8080->8080/tcp   container-PROXY-lazamar
> 2a19bd611e96        lazamar:0.5         ...         8080/tcp                         container-Blue-lazamar
```

**Here comes the cool part**. To swap your current image to a newer version, just run it with the new version number

```
easy-deploy -p 8080:8080 lazamar:0.6
>
> Starting Green image.
> Waiting for 3 seconds for server to start
> Switching proxy to Green
> Waiting for 3 seconds for Blue server to finish handling its requests
> Blue container killed
> Green is now the main container
```

That's it. Now version 0.6 went is running on port 8080 and no requests were lost.

```
docker ps

>> CONTAINER ID        IMAGE               PORTS                            NAMES
>> e703e8567995        lazamar:0.6         8080/tcp                         container-Green-lazamar
>> c0345092b90c        nginx:latest        80/tcp, 0.0.0.0:8080->8080/tcp   container-PROXY-lazamar
```

## How it works

`easy-deploy` will run an nginx proxy server that will redirect requests on the ports specified to your image.

When you run a new version of your image, it will switch the currently running one with the new one without letting any request drop.

Currently you cannot set the name of the container. An automatic container name will be generated based on the image name and the current active deployment color (blue or green).


## Releases

### v0.1.0
    -   Run target docker image linking ports and volumes
    -   Accept commands to send to image

## TODO

    -   Accept a suite of
