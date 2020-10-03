var tty = require('tty');  
var Elm = require('./main').Elm;
var XMLHttpRequest = require("xmlhttprequest").XMLHttpRequest;

var stdin = process.stdin;

// without this, we would only get streams once enter is pressed
stdin.setRawMode( true );

// resume stdin in the parent process (node app won't quit all by itself
// unless an error or process.exit() happens)
stdin.resume();

// i don't want binary, do you?
stdin.setEncoding( 'utf8' );


var clear_screen = function () {
    process.stdout.cursorTo(0,0);
    process.stdout.clearScreenDown();
};

clear_screen();
var main = Elm.Main.init( { flags: { 'rows': process.stdout.rows } }) ;


// on any data into stdin
stdin.on( 'data', function( key ){
    // ctrl-c ( end of text )
    if ( key === '\u0003' ) {
        process.exit();
    }
    // write the key to stdout all normal like
    // process.stdout.write( key );
    main.ports.get.send( key );

});

main.ports.put.subscribe(
    function (data){
        clear_screen();
        process.stdout.write (data);
});

main.ports.quit.subscribe(
    function (data){
        process.stdout.write("\n");
        process.stdout.write (data);
        process.exit();
});
