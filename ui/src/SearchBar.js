import {React} from 'react';
import {SearchOutlined} from '@ant-design/icons';
import {Input} from 'antd';

const SearchBar = () => {
    return (
        <Input
            id="searchbar"
            allowClear
            placeholder="Search"
            prefix={<SearchOutlined className="site-form-item-icon" />}
        />
    );
};

export default SearchBar;
