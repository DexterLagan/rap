# RAP - Racket on Bootstrap
Combination of Racket and Bootstrap, RAP is a Web framework aiming to produce good-looking pages with ease.

## Design goals

- easy to use
- pleasing pages through Bootstrap
- routing capability
- good performance if possible


## How to use
1) update the database configuration file:
<pre>
nano db.conf
</pre>
2) start the server with:
<pre>
sudo racket rap.rkt
</pre>
3) point your browser to:
<pre>
http://localhost/index.cgi
</pre>

## TODO / Status

- implement HTML template page with Boostrap headers - DONE;
- implement template display code - DONE;
- implement temporary file generation code to enable developer's preview - DONE;
- implement utility functions - in progress;
- implement OS detection and switching;
- implement database configuration file parser - in progress;
- port Newstrap code from newLISP to Racket - in progress;
- test macro code on 8.2 and 8.2BC for performance eval.;
- research on routing (https://docs.racket-lang.org/routy/index.html is a good start);
- implement forms (https://docs.racket-lang.org/forms/index.html)
- look into formlets (https://docs.racket-lang.org/web-server/formlets.html)
- include configuration file library (https://rosettacode.org/wiki/Read_a_configuration_file#Racket, https://rosettacode.org/wiki/Racket/Options) - DONE;
- implement database connectivity using newstrap as ref. (https://docs.racket-lang.org/db/using-db.html);
- look into cookies, sessions, forms, database connections, and static files (see https://www.monolune.com/simple-web-applications-in-racket/).

## Customization

For further customization, see the following pages for sample content:
http://bootstrapdocs.com/v3.3.4/docs/examples/theme/
http://bootstrapdocs.com/v3.3.4/docs/getting-started/#examples

Use following links for CDN-hosted CSS, Theme, jQuery and Javascript:
https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css
https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap-theme.min.css
https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js
https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js

## License

RAP is free software; see [LICENSE](https://github.com/DexterLagan/rap/blob/master/LICENSE) for more details.
