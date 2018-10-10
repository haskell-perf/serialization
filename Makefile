clean:
	 find . -name "*.dump*" -exec rm -- {} +

run:
	stack bench :all
	#stack build --exec serialization-bench --file-watch

dev:
	stack test serialization:docs --file-watch	--fast