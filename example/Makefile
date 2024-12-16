export VERSION ?=$(shell git rev-parse HEAD)

DEPLOYMENTDIR=deployment
BUILDDIR=build-resources

# Check that given variables are set and all have non-empty values,
# die with an error otherwise.
#
# Params:
#   1. Variable name(s) to test.
#   2. (optional) Error message to print.
check_defined = \
	$(strip $(foreach 1,$1, \
		$(call __check_defined,$1,$(strip $(value 2)))))
	__check_defined = \
		$(if $(value $1),, \
			$(error Undefined $1$(if $2, ($2))))

validate-env:
	$(call check_defined, ENV, which environment to use)


build-config-files-service: validate-env
	rm -rf $(BUILDDIR)
	mkdir  $(BUILDDIR)
	porter 	--env ${ENV} \
		--src $(DEPLOYMENTDIR)/templates/config.ig.template.edn \
		--dest $(BUILDDIR)/config.ig.edn \
		--ctx-paths $(DEPLOYMENTDIR)/ctx/base.edn $(DEPLOYMENTDIR)/ctx/secrets.edn \
		--classpaths $(DEPLOYMENTDIR)/clj \
		--injections '{:version "$(VERSION)"}' \
		--print true \
		--namespaces helper \


build-docker-anchorpoint: validate-env
	porter 	--env ${ENV} \
		--src "docker build -f deployment/docker/Dockerfile.anchorpoint  -t {{:docker.image/anchorpoint+env}} ." \
		--ctx-paths $(DEPLOYMENTDIR)/ctx/base.edn \
		--classpaths $(DEPLOYMENTDIR)/clj \
		--namespaces helper \
		--print true \
		--exec true \

	porter 	--env ${ENV} \
		--src "docker image tag {{:docker.image/anchorpoint+env}} {{:docker.image/anchorpoint+version}}" \
		--ctx-paths $(DEPLOYMENTDIR)/ctx/base.edn \
		--classpaths $(DEPLOYMENTDIR)/clj \
		--namespaces helper \
		--injections '{:version "$(VERSION)"}' \
		--print true \
		--exec true \
