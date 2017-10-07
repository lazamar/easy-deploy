

watch:
	stack build --file-watch --pedantic

run:
	stack exec easy-deploy-exe




## ---------------

NETWORK=easy-deploy
NAME1=container-Blue-lazamar.co.uk
NAME2=container-Green-lazamar.co.uk
NGINX=server-nginx

start-handler:
	docker run \
		-v $(shell pwd)/nginx/conf.d/:/etc/nginx/conf.d/ \
		--name=$(NGINX) \
		-p 8080:8080 \
		--net=$(NETWORK) \
		-d \
		nginx:latest

start-sample1:
	docker run \
		-v /home/marcelo/Programs/Projects/lazamar.co.uk:/home/app \
		--name=$(NAME1) \
		-d \
		--net=$(NETWORK) \
		lazamar.co.uk

start-sample2:
	docker run \
		-v /home/marcelo/Programs/Projects/lazamar.co.uk:/home/app \
		--name=$(NAME2) \
		-d \
		--net=$(NETWORK) \
		lazamar.co.uk

start:
	make stop
	make start-sample1
	make start-sample2
	make start-handler

stop:
	docker kill $(NGINX); \
	docker rm -f $(NGINX); \
	docker kill $(NAME1); \
	docker rm -f $(NAME1); \
	docker kill $(NAME2); \
	docker rm -f $(NAME2); \
	exit 0
