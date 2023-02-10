package com.minioservice.enums;

public enum FileOperateEnum {
    /** 0-查询，1-上传，2-下载，3-删除 */
    SELECT("0"),
    UPLOAD("1"),
    DOWNLOAD("2"),
    DELETE("3"),
    TAGSMODIFY("4");

    final String val;
    private FileOperateEnum(String val) {
        this.val = val;
    }

    public String getVal() {
        return this.val;
    }
}
