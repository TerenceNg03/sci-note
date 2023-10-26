import {React} from 'react';
import {Menu} from 'antd';

const menuItems = [
    getItem('Favorite', 'favorite', null, [getItem('Option 13', '0'), getItem('Option 14', '1')]),
    getItem('Tags', 'tags', null, [getItem('Option 13', '5'), getItem('Option 14', '6')]),
    getItem('Recent', 'recent', null, [getItem('Option 13', '3'), getItem('Option 14', '4')]),
];

function getItem(label, key, icon, children, type) {
    return {key, icon, children, label, type};
}

const QuickAccess = () => {
    return (
        <Menu
            items={menuItems}
            mode='inline'
            defaultOpenKeys={['favorite', 'recent', 'tags']}
            inlineIndent='15'
            style={{
                height: '100vh',
                overflowY: 'scroll',
                scrollbarWidth: 'none',
                backgroundColor: 'transparent',
            }}
        />
    )
}

export default QuickAccess;
