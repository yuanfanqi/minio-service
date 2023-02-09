package com.minioservice.enums;

/**
 * @Description: 常用枚举类
 * @Author: song
 * @Date: 2023/2/8 14:53
 */
public enum CommonEnum {
    /** 否or失败 */
    FALSE("0"),
    /** 是or成功 */
    TRUE("1"),
    /** 失败 */
    E("E"),
    /** 成功 */
    S("S");

    final String statusVal;

    private CommonEnum(String statusVal) {
        this.statusVal = statusVal;
    }

    public String getStatusVal() {
        return this.statusVal;
    }
}
