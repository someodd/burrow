# add make network? setup? etc?
running_id := $$(docker ps --filter ancestor=spacecookie --format "{{.ID}}")
number_running := $$( echo "$running_id" | wc -l )

all:
	set -e;

create_network:
	docker network create --subnet=172.18.0.0/16 servicenet

build:
	docker build -t spacecookie .

run:
	docker run -d --restart=always --net servicenet --hostname=spacecookie --ip=172.18.0.68 spacecookie

test:
	echo "$(running_id)"
	echo "$(number_running)"
	echo "$$(echo $(running_id) | head -n 1)"

shell:
	@if [ ${number_running} -gt 1 ] ; then echo "More than one spacecookie descendant container found."; exit 1; fi
	@if [ ${number_running} -eq 0 ]; then echo "No spacecookie descendant container found."; exit 1; fi
	docker exec -it "$$( echo "$(running_id)" | head -n 1)" bash
