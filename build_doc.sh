rm -rf yaml.docdir
cp myocamlbuild_doc.ml myocamlbuild.ml
ocamlbuild src/yaml.docdir/index.html
rm myocamlbuild.ml
