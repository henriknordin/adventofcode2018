help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid: ## Run ghcid with the adventofcode2018 project
	ghcid \
	    --command "stack ghci adventofcode2018 --ghci-options='-fwarn-unused-imports -fwarn-unused-binds'"

.PHONY: ghcid help
