$(function() {
    var star = '.s', hover = 's-ho', selector = 'a span.s';
    $(selector).live('mouseenter', function() {
        $(this).addClass(hover).parent().prevAll().find(star).addClass(hover);
    });
    $(selector).live('mouseleave', function() {
        $(this).removeClass(hover).parent().prevAll().find(star).removeClass(hover);
    });
});
