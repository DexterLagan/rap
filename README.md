# RAP - Racket on Bootstrap
Combination of Racket and Bootstrap, RAP is a Web framework aiming to produce good-looking pages with ease.

## Design goals

- easy to use
- pleasing pages through Bootstrap
- routing capability
- good performance if possible

## Proposed configuration steps
1) update the database configuration file:
<pre>
vi db.conf
</pre>
2) start the server with:
<pre>
/usr/local/bin/rap
</pre>
3) point your browser to:
<pre>
http://localhost/index.cgi
</pre>

## Project Status

### What's done
- implement HTML template page with Boostrap headers;
- implement template display code;
- implement temporary file generation code to enable developer's preview;
- implement utility functions;
- implement database configuration file parser;
- port Newstrap code from newLISP to Racket;
- include configuration file library (https://rosettacode.org/wiki/Racket/Options).

### What's missing
- implement OS detection and switching;
- test macro code on 8.2 and 8.2BC for performance eval.;
- research on routing (https://docs.racket-lang.org/routy/index.html is a good start);
- implement forms (https://docs.racket-lang.org/forms/index.html)
- look into formlets (https://docs.racket-lang.org/web-server/formlets.html)
- implement database connectivity using newstrap as ref. (https://docs.racket-lang.org/db/using-db.html);
- look into cookies, sessions, forms, database connections, and static files (see https://www.monolune.com/simple-web-applications-in-racket/).

## Configuration
Here's a sample configuration file:
<pre>
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

db-connection-name rap MySQL
db-username root
db-password 12345
db-port 3306
db-name rap

# EOF
</pre>

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
