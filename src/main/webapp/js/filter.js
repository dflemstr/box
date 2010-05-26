$(function() {
    $('.changer').change(function() {
        var result = '', elems = [],
            keywords = '#keywords-field',
            title = '#title-field',
            description = '#description-field',
            category = '#category-field',
            major = '#major-field',
            minor = '#minor-field',
            release = '#release-field',
            build = '#build-field';
        if($(keywords).val())
            result += $(keywords).val() + ' ';
        if($(title).val()) {
            elems = $(title).val().split(' ');
            for(elem in elems) {
                result += ('title:' + elems[elem].trim() + ' ');
            }
        }
        if($(description).val()) {
            elems = $(description).val().split(' ');
            for(elem in elems) {
                result += ('description:' + elems[elem].trim() + ' ');
            }
        }
        if($(category).val()) {
            elems = $(category).val().split(' ');
            for(elem in elems) {
                result += ('category:' + elems[elem].trim() + ' ');
            }
        }
        if($(major).val() && $(minor).val() && $(release).val() && $(build).val())
            result += ('version:' + $(major).val() + '.' + $(minor).val() + '.' + $(release).val() + '.' + $(build).val() + ' ');
        else {
            if($(major).val())
                result += ('major:' + $(major).val() + ' ');
            if($(minor).val())
                result += ('minor:' + $(minor).val() + ' ');
            if($(release).val())
                result += ('release:' + $(release).val() + ' ');
            if($(build).val())
                result += ('build:' + $(build).val() + ' ');
        }
        $('#filter-field').val(result.trim());
    });
});
