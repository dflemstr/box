
var ratingAdded = function(id, val, count) {
    var fast = 'fast';
    $('#' + id).stars('select', val).stars('disable').parent().find('.rating-count')
    .text(count).fadeOut(fast).fadeIn(fast).fadeOut(fast)
    .fadeIn(fast).fadeOut(fast).fadeIn(fast);
}

$(function() {
    $('.rating').each(function() {
        var ajaxId = $(this).attr('onchange').match(/'([A-Z0-9]+=)'/)[1];
        $(this).parent().stars({
            inputType: "select",
            split: 2,
            cancelShow: false,
            callback: function(ui, type, value) {
                liftAjax.lift_ajaxHandler(ajaxId + value, null, null, null);
            }
        });
    });
});
