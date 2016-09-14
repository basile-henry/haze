rm -rf dist
mkdir dist
cp src/index.html dist/
cp res/favicon.png dist/
elm make src/Main.elm --yes --output dist/temp.js
uglifyjs dist/temp.js --compress --output dist/main.js &> /dev/null
rm dist/temp.js
echo "Done!"
