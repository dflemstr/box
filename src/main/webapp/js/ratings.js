$(function() {
    $('a span.star').hover(function() {
        $(this).addClass('star-hover').parent().prevAll().find('.star').addClass('star-hover');
    }, function() {
        $(this).removeClass('star-hover').parent().prevAll().find('.star').removeClass('star-hover');
    });
});
