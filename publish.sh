git checkout -b temp
sed '/dist/,+1 d' < .gitignore > temp
mv temp .gitignore
./build.sh
git add .gitignore
git add dist
git commit -m "Latest build"
git subtree split --prefix dist -b gh-pages
git checkout gh-pages
git push -f --set-upstream origin gh-pages
git checkout master
git branch -D temp
git branch -D gh-pages
