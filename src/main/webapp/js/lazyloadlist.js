var bindLoadEvent = function(toId, callback) {
    $('#' + toId).one('loadmore', callback);
}

$(function() {
    var nativeWind = window,
    wind = $(nativeWind),
    maybeLoadMore = function() {
        var loader = $('#lazy-loader').parent(),
        margin = 200,
        top = wind.scrollTop() - margin,
        windJQHeight = wind.height(),
        bottom = top + ((windJQHeight == 0) ? nativeWind.innerHeight : windJQHeight) + margin, //Height workaround for Chrome 5 and 6
        loaderTop = loader.offset().top;
        if(loaderTop > top && loaderTop < bottom) {
            loader.trigger('loadmore');
        }
    };
    wind.bind('scroll resize', maybeLoadMore);
    $(maybeLoadMore);
});
