stack exec site rebuild

\cp -fr _site /tmp

git checkout gh-pages

git pull

\cp -fr /tmp/_site/* .

git add .

git commit -m "Publish site"

git push origin gh-pages
